# splits data by manually defined hydrograph bounds and calculated percentage of total export before discharge peak

hysteresis_func <- function(GS1, hydro_sep) {
  output_main <- tibble("year" = NA, "JD" = NA, "chunk" = NA, Qw_cs = NA, SSC_cs = NA)

  for (y in c(2020, 2021, 2022)) {
    k <- if (y == 2022) {
      12
    } else {
      30
    }

    sep <- hydro_sep |>
      filter(year == y) |>
      ungroup()

    data <- GS1 |>
      dplyr::filter(year == y) |>
      mutate(SSC = (Qss * 10^3) / (dt * 60) / Qw) |>
      filter(SSC > 0 & Qw > 0) |>
      ungroup() |>
      select(year, JD, dt, Qw, SSC, Qss, Qb) |>
      mutate(
        chunk = cut(JD, breaks = sep$JD),
        chunk = as.integer(str_extract(chunk, "\\d+"))
      ) |>
      mutate(year = y)

    for (d in unique(data$chunk)) {
      suppressWarnings({
        SSC_cs <- data |>
          filter(chunk == d) |>
          mutate(
            Qw = rescale(Qw |> caTools::runmean(k = k)),
            SSC = rescale(SSC |> caTools::runmean(k = k)),
            Qss = rescale(Qss |> caTools::runmean(k = k)),
            Qw_cs = cumsum(Qw) |> rescale(),
            SSC_cs = cumsum(SSC) |> rescale()
          )

        Qb_cs <- data |>
          filter(chunk == d) |>
          arrange(year, JD) |>
          drop_na(Qw, Qb) |>
          mutate(Qb_load = (((Qb * 1000) / (dt * 60)) / Qw) * glacier_area) |>
          drop_na(Qb) |>
          mutate(
            Qw = rescale(Qw |> caTools::runmean(k = k)),
            Qb = rescale(Qb |> caTools::runmean(k = k)),
            Qw_cs = cumsum(Qw) |> rescale(),
            Qb_cs = cumsum(Qb) |> rescale(),
            Qb_load = rescale(Qb_load)
          )
      })

      t_Qw_max_SSC <- SSC_cs |>
        filter(Qw == 1) |>
        slice_head(n = 1)

      t_Qw_max_Qb <- Qb_cs |>
        filter(Qw == 1) |>
        slice_head(n = 1)

      t_Qw_max <- tibble(
        year = t_Qw_max_SSC$year,
        JD = t_Qw_max_SSC$JD,
        chunk = t_Qw_max_SSC$chunk,
        Qw_cs = t_Qw_max_SSC$Qw_cs,
        SSC_cs = t_Qw_max_SSC$SSC_cs,
        Qb_cs = t_Qw_max_Qb$Qb_cs
      )

      output_main <- output_main |>
        bind_rows(t_Qw_max) |>
        drop_na()
      e <- floor(t_Qw_max$JD)
      # print(paste0(e, ".", y))
    }
  }

  HI <- output_main |>
    mutate(
      Bedload = Qb_cs,
      Suspended = SSC_cs
    ) |>
    select(year, JD, chunk, Bedload, Suspended) |>
    rename(day = chunk) |>
    pivot_longer(cols = contains("ed"), names_to = "var") |>
    mutate(
      day = round(JD) + 1.5,
      HI = if_else(value >= 0.5, "C", "A"),
      HI = as.factor(HI),
      year = factor(year, levels = c("2020", "2021", "2022")),
      var = if_else(var == "Suspended", "S", "B"),
      var = as.factor(var),
      value = if_else(value < 0.6 & value > 0.4, 0.5, value)
    ) |>
    select(year, day, var, value, HI)

  HI_AC <- HI |>
    filter(HI == "A") |>
    mutate(value = rescale(value, to = c(-1, 0)))
  HI_C <- HI |>
    filter(HI == "C") |>
    mutate(value = rescale(value, to = c(0, 1)))

  HI <- full_join(HI_AC, HI_C)

  return(HI)
}
