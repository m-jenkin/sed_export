# downscales high res DEMs of surface and bed, then calculates shreve. 
# flow acc applied afterwards and thresholded to extract channels

shreve_func <- function(glacier_outline, surface, bed, agg, dir) {
  
  pw <- 1000 # water density in kg/m^3
  pi <- 910 # density of ice in kg/m^3
  g <- 9.81 # gravitational acceleration in m/s
  c <- 1 # pressurization constant
  
    rast(surface) |> 
      mask(glacier_outline) |>
      aggregate(agg, fun = 'mean') |> 
      writeRaster(paste0(dir, 'surface2.tif'), overwrite = T)
    
    wbt_fill_depressions(dem = paste0(dir, 'surface2.tif'),
                         output = paste0(dir, 'surface2.tif'))
    
    rast(bed) |> 
      mask(glacier_outline) |>
      aggregate(agg, fun = 'mean') |> 
      writeRaster(paste0(dir, 'bed2.tif'), overwrite = T)
    
    wbt_fill_depressions(dem = paste0(dir, 'bed2.tif'),
                         output = paste0(dir, 'bed2.tif'))
    
    surface <- rast(paste0(dir, 'surface2.tif'))
    bed <- rast(paste0(dir, 'bed2.tif'))
    thickness <- surface - bed
    shreve <- (pw * g * bed) + (c * ((pi * g) * (surface - bed)))
    
    writeRaster(shreve, filename = paste0(dir, 'shreve.tif'), overwrite = T)
    
    wbt_fill_depressions(dem = paste0(dir, 'shreve.tif'),
                         output = paste0(dir, 'shreve.tif'))
    
    wbt_fill_single_cell_pits(dem = paste0(dir, 'shreve.tif'),
                              output = paste0(dir, 'shreve.tif'))
    
    wbt_d8_flow_accumulation(input = paste0(dir, 'shreve.tif'),
                             output = paste0(dir, 'shreve.tif'),
                             out_type = 'cells',
                             log = F)
    
    d8fa <- terra::rast('./data/temp/shreve.tif') |> 
      as.data.frame(xy = TRUE) |> 
      pivot_longer(cols = starts_with('s'), 
                   names_to = 'layer', 
                   values_to = 'value')
  
  return(d8fa)
  
}