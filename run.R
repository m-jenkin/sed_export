# Matt Jenkin, University of Lausanne 2024

# Load packages and custom functions --------------------------------------

# install.packages('pacman')
pacman::p_load(
  readr, tidyr, dplyr, stringr, forcats, purrr, ggplot2, fs,
  scales, GGally, caTools, sf, terra, whitebox, conflicted
)
conflict_prefer("extract", "terra")
conflict_prefer("rescale", "scales")
conflict_prefer("select", "dplyr")
map(list.files("./functions/", full.names = T), source)

# Read data and calculate daily summaries ---------------------------------

glacier_area <- 7.907 # km2
glacier_outline <- read_sf("./data/otemma_outline_2021.gpkg") |>
  st_union() |>
  vect()

meteo_daily <- read_csv("./data/meteo_daily.csv") |>
  mutate(year = as.factor(year))

GS1 <- read_csv("./data/export.csv") |>
  filter((year == 2020 & day >= 189 & day <= 242) |
    (year == 2021 & day >= 164 & day <= 252) |
    (year == 2022 & day >= 137 & day <= 243)) |>
  mutate(year = as.factor(year))

GS1_daily <- GS1 |>
  mutate(across(c(Qw, Qw_lo, Qw_hi), ~ .x * (dt * 60))) |> # yield to flux
  group_by(year, day) |>
  summarise(across(contains(c("Qw", "Qss", "Qb")),
    ~ sum(.x / glacier_area, na.rm = T), # normalise by glacier area
    .names = "{.col}"
  )) |>
  ungroup() |>
  mutate(across(matches("Qw"), ~ .x / 10^6), # million cumecs
    Qs = Qss + Qb, # total daily export
    Qs_lo = Qss_lo + Qb_lo,
    Qs_hi = Qss_hi + Qb_hi
  ) |>
  filter((year == 2020 & day >= 189 & day <= 242) | # days with reliable Qw, Qss and Qb
    (year == 2021 & day >= 164 & day <= 252) |
    (year == 2022 & day >= 137 & day <= 243)) |>
  left_join(meteo_daily |> select(-date), by = c("year", "day")) |>
  left_join(entropy_func(GS1)) |> # discharge entropy
  mutate(entropy = rescale(entropy))

sep <- read_csv("./data/hydrographs.csv") |>
  mutate(year = as.factor(year))

# Data inspection ---------------------------------------------------------

GS1_daily_long <- GS1_daily |>
  pivot_longer(cols = !matches(c("year", "day")))

ggplot(GS1_daily_long, aes(day, value, col = year)) + # all variables
  geom_point(pch = 3, size = 0.7) +
  facet_wrap(~name, scales = "free_y") +
  ggtitle("daily dataset")

GS1 |> # check the hydrograph separation (done manually)
  group_by(year) |>
  select(year, JD, Qw) |>
  mutate(
    hydro = cut(JD, breaks = unique(sep$JD)), # splitting
    hydro = as.factor(str_extract(hydro, "\\d+"))
  ) |>
  ggplot(aes(JD, Qw, col = hydro)) +
  geom_line(show.legend = F) +
  facet_wrap(~year, nrow = 3, scales = "free_y") +
  ggtitle("hydrograph splitting")

# Melt season summary statistics ------------------------------------------

# basin averaged totals
export_totals <- GS1 |>
  group_by(year) |>
  mutate(across(contains(c("Qw")), ~ .x * (dt * 60) / 10^6 / glacier_area)) |> # yield in million cumecs per km2
  mutate(across(contains(c("Qss", "Qb")), ~ .x / glacier_area)) |> # already in tons/dt
  reframe(
    Qw = sum(Qw, na.rm = T),
    Qw_lo = sum(Qw_lo, na.rm = T),
    Qw_hi = sum(Qw_hi, na.rm = T),
    Qw_unc = Qw_hi - Qw,
    Qss = sum(Qss, na.rm = T),
    Qss_lo = sum(Qss_lo, na.rm = T),
    Qss_hi = sum(Qss_hi, na.rm = T),
    Qss_unc = Qss - Qss_lo,
    Qb = sum(Qb, na.rm = T),
    Qb_lo = sum(Qb_lo, na.rm = T),
    Qb_hi = sum(Qb_hi, na.rm = T),
    Qb_unc = Qb - Qb_lo,
    Qb_unc_hi = Qb_hi - Qb,
    Qs = Qss + Qb,
    Qs_lo = Qss_lo + Qb_lo,
    Qs_hi = Qss_hi + Qb_hi,
    Qs_unc_lo = Qs - Qs_lo,
    Qs_unc_hi = Qs_hi - Qs,
    percent_bedload = (Qb / Qs) * 100
  ) |>
  pivot_longer(cols = !matches("year"))

# instantaneous flux
flux_stats <- GS1 |>
  group_by(year) |>
  mutate(across(contains(c("Qss", "Qb")), ~ (.x * 10^3) / (dt * 60) / Qw)) |> # tons/dt to kg/s
  summarise(
    across(c(Qw, Qss, Qb),
      list(
        median = ~ median(.x, na.rm = T),
        mean = ~ mean(.x, na.rm = T),
        max = ~ max(.x, na.rm = T),
        mean_unc = ~ mean(.x - get(paste0(cur_column(), "_lo")), na.rm = T)
      ),
      .names = "{.fn}_{.col}"
    ),
    median_Qb_unc = median(Qb_hi - Qb_lo, na.rm = T),
    mean_Qs = mean(Qss + Qb, na.rm = T),
    median_Qs = median(Qss + Qb, na.rm = T),
    max_Qs = max(max_Qss + max_Qb)
  ) |>
  pivot_longer(cols = -year)

# weather summaries for melt season and Apr-Sept
meteo_summaries <- meteo_daily |>
  group_by(year) |>
  filter(day >= 105 & day <= 250 & mean_temp > 0) |>
  summarise(PDD_sum = sum(mean_temp, na.rm = T)) |>
  full_join(GS1_daily |>
    group_by(year) |>
    filter((year == 2020 & day >= 189 & day <= 242) |
      (year == 2021 & day >= 164 & day <= 252) |
      (year == 2022 & day >= 137 & day <= 243)) |>
    filter(mean_temp > 0) |>
    mutate(sum_precip = if_else(is.na(sum_precip), 0, sum_precip)) |>
    summarise(
      mean_mean_temp = mean(mean_temp, na.rm = T),
      mean_rainfall = mean(sum_precip, na.rm = T),
      sum_rainfall = sum(sum_precip, na.rm = T)
    ))

# scatter matrix including Spearman correlations
GS1_daily |>
  select(!contains(c("day", "lo", "hi", "date", "SLA", "cover"))) |>
  ggpairs(aes(colour = factor(year), alpha = 0.6),
    upper = list(continuous = GGally::wrap("cor", method = "spearman", size = 2)),
    lower = list(continuous = GGally::wrap(GGally::ggally_points, size = 0.35))
  ) +
  ggtitle("scatter matrix of daily data")

# Water-sediment hysteresis -----------------------------------------------

HI <- hysteresis_func(GS1, sep)

ggplot(HI) +
  geom_point(aes(x = day, y = var), colour = "darkgrey") +
  geom_tile(aes(x = day, y = var, fill = HI, height = abs(value)), width = 1) +
  facet_wrap(~year, nrow = 3) +
  ggtitle("water-sediment hysteresis direction")

# Export by SLA band ------------------------------------------------------

band_width <- 50

export_by_SLA <- export_by_SLA_func(
  data = {
    GS1_daily |> rename("SLA" = SLA_min, "SCAR" = cover)
  },
  dem = "./data/surface.tif",
  glacier_outline = "./data/otemma_outline_2021.gpkg",
  band_width = band_width,
  hysteresis = HI
)

snow_max <- GS1_daily |>
  filter((year == 2020 & day >= 189 & day <= 242) |
    (year == 2021 & day >= 164 & day <= 252) |
    (year == 2022 & day >= 137 & day <= 243)) |>
  group_by(year) |>
  mutate(year = as.factor(year)) |>
  summarise(max = max(SLA_min, na.rm = T))

ggplot(
  export_by_SLA,
  aes(value, SLA_band + (band_width * 0.5), colour = year, lwd = period)
) +
  geom_hline(data = snow_max, aes(yintercept = max, colour = year)) +
  geom_path() +
  facet_wrap(~variable, scales = "free_x", nrow = 1) +
  ggtitle("sediment exported by SLA band")


# Cumulative data with uncertainty -----------------------------------------


cs_Qss <- GS1_daily |>
  group_by(year) |>
  arrange(year, day) |>
  mutate(
    Qss_cs = cumsum(Qss),
    Qb_cs = cumsum(Qb),
    Qw_cs = cumsum(Qw)
  ) |>
  select(year, day, Qss_cs, Qb_cs, Qw_cs) |>
  mutate(across(contains("Q"), ~ rescale(.x))) |>
  pivot_longer(cols = c("Qss_cs", "Qb_cs"), names_to = "var", values_to = "value")

ggplot(cs_Qss, aes(Qw_cs, value, colour = year, lwd = day)) +
  geom_line() +
  facet_wrap(~var) +
  ggtitle("cumulative export by cumulative discharge")

p75 <- GS1 |>
  group_by(year) |>
  mutate(
    p75 = quantile(Qw, 0.75, na.rm = T),
    total_Qss = sum(Qss, na.rm = T),
    total_Qb = sum(Qb, na.rm = T)
  )
p75a <- p75 |>
  group_by(year) |>
  summarise(
    sum_Qss = sum(Qss, na.rm = T),
    sum_Qb = sum(Qb, na.rm = T)
  )
p75b <- p75 |>
  group_by(year) |>
  filter(Qw < p75) |>
  summarise(
    sum_Qss_below_p75 = sum(Qss, na.rm = T),
    sum_Qb_below_p75 = sum(Qb, na.rm = T)
  )
proportions <- p75a |>
  left_join(p75b, by = "year") |>
  mutate(
    proportion_Qss = sum_Qss_below_p75 / sum_Qss,
    proportion_Qb = sum_Qb_below_p75 / sum_Qb
  ) |>
  select(year, proportion_Qss, proportion_Qb) |>
  pivot_longer(cols = !matches("year"))

ggplot(proportions, aes(name, value, col = year)) +
  geom_point(pch = 3, size = 10, stroke = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  ggtitle("proportion exported below 75th percentile discharge")

cs_GS1 <- GS1_daily |> # some wrestling to plot cumulative uncertainty without tons of individual plot code
  group_by(year) |>
  arrange(year, day) |>
  mutate(across(contains("Q"), ~ cumsum(.x))) |>
  select(1:14) |>
  pivot_longer(cols = matches("Q[^_]*(?:_lo|_hi)?$"), names_to = "var", values_to = "value") |>
  separate(var, into = c("var_name", "var_type"), sep = "_", fill = "right") |>
  mutate(var_type = if_else(is.na(var_type) | var_type == "", "value", var_type)) |>
  pivot_wider(names_from = var_type, values_from = value, names_glue = "var_{var_type}") |>
  mutate(var = str_extract(var_name, "Q[^_]*"))

ggplot(cs_GS1, aes(x = day, group = var)) +
  geom_ribbon(aes(ymin = var_lo, ymax = var_hi, fill = year), alpha = 0.2) +
  geom_line(aes(y = var_value, color = year)) +
  facet_wrap(~ interaction(year, var), scales = "free", nrow = 4) +
  ggtitle("Cumulative data with uncertainty ranges")

# Subglacial channel flow routing - Shreve --------------------------------

fs::dir_create("./data/temp/")
shreve <- shreve_func(glacier_outline,
  surface = "./data/surface.tif",
  bed = "./data/bed.tif",
  agg = 5, # downsampling
  dir = "./data/temp/"
) # location for temp files
fs::dir_delete("./data/temp/")

ggplot(shreve |> filter(value > 0)) +
  geom_raster(aes(x, y, fill = log(value))) +
  coord_fixed() +
  scale_fill_viridis_c(option = "G") +
  ggtitle("d8 flow accumulation based on hydraulic potential")
