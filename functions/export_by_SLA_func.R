# Function first determines the glacier surface area within elevation bands with size band_width.
# Uses swisstopo 2m DEM and custom drawn outline of main glacier tongue.
# SLA then linearly interpolated to 1 day resolution, export data split by elevation band, summary yield data calculated per band

export_by_SLA_func <- function(data, dem, glacier_outline, band_width, hysteresis) {
  dem <- rast(dem)
  glacier_outline <- read_sf(glacier_outline) |>
    slice(2) |>
    select(RIGHT_FID) |>
    terra::vect() # read, clean and convert gpkg
  crs(glacier_outline) <- crs(dem) # set identical CRS
  glacier_area <- st_area(glacier_outline |> st_as_sf()) |>
    round() |>
    as.numeric()
  res <- res(dem)[1] # dem cell size
  bands <- tibble(band = as.character(seq(2500, 3600, band_width))) # generate elevation bands
  
  elevation_bands <-
    extract(dem, glacier_outline, touches = F) |> # extract cells inside glacier outline
    mutate(count = cut_interval(surface, l = band_width, dig.lab = 5)) |> # split by elevation band
    group_by(count) |>
    summarise(n = n()) |> # count cells in each band
    mutate(total_area = n * res^2) |> # multiply cell count per SLA band by DEM resolution for area
    mutate(band = substr(count, 2, 5)) |> # extract band names
    right_join(bands, by = "band") |> # add in band metadata
    mutate(proportion = as.numeric(total_area) / as.numeric(glacier_area) * 100, # proportion of glacier covered by band
           band_numeric = as.numeric(band),
           cs = cumsum(proportion)) |> # cumulative proportion
    drop_na() |>
    select(band, band_numeric, total_area, proportion, cs) |>
    mutate(facet = "Hypsometry")
  
  SLA_interpolated <- data |> # linear interpolation of SLA to 1 day resolution
    left_join(hysteresis, by = c("year", "day")) |>
    group_by(year) |>
    complete(day = seq(min(day), max(day), 1)) |>
    mutate(index = seq_along(day),
           SLA = ifelse(is.na(SLA),
                        approx(index[!is.na(SLA)], SLA[!is.na(SLA)], index)$y, SLA),
           SCAR = ifelse(is.na(SCAR),
                         approx(index[!is.na(SCAR)], SCAR[!is.na(SCAR)], index)$y, SCAR),
           SLA = round(SLA, digits = 2),
           SCAR = round(SCAR, digits = 2)) |>
    select(-index) |>
    drop_na(SLA, SCAR) |>
    mutate(SLA_band = 
             cut(SLA,
                 breaks = seq(from = floor(min(SLA + 2, na.rm = TRUE) / band_width) * band_width, # assign the interpolated SLA to a band_widthm band
                              to = ceiling(max(SLA + 2, na.rm = TRUE) / band_width) * band_width,
                              by = band_width),
                 include.lowest = TRUE,
                 labels = paste(seq(from = floor(min(SLA + 2, na.rm = TRUE) / band_width) * band_width,
                                    to = ceiling(max(SLA + 2, na.rm = TRUE) / band_width) * band_width - band_width,
                                    by = band_width))))
  
  export_by_band <- SLA_interpolated |>
    mutate(SLA_band = as.numeric(paste(SLA_band))) |> 
    group_by(year, SLA_band) |>
    summarise(period = round(quantile(day, 0.9, na.rm = T)), 
              days = length(day), .groups = "drop",
              sumQw = sum(Qw, na.rm = T),
              sumQb = sum(Qb, na.rm = T), # total Qb etc. per band
              sumQss = sum(Qss, na.rm = T),
              sumY = (sum(Qb, na.rm = T) + sum(Qss, na.rm = T))) |> # n days SLA in each band |>
    arrange(year, SLA_band) |> 
    group_by(year) |> 
    mutate(cs_Qw = cumsum(sumQw),
           cs_Qss = cumsum(sumQss),
           cs_Qb = cumsum(sumQb)) |>
    mutate(across(contains(c("cs_Qss", 'cs_Qb')), ~ .x / 10^3)) |>
    group_by(year) |> 
    pivot_longer(cols = !matches(c("year", "SLA_band", "period", "days")), names_to = "variable", values_to = "value") |>
    mutate(variable = fct_relevel(variable, "cs_Qw", "cs_Qss", "cs_Qb"),
           year = as.factor(year)) |>
    arrange(SLA_band) |> 
    filter(str_detect(variable, 'cs'))
  
  return(export_by_band)
}

