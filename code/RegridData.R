#Function to regrid data from a fine resoliution to a coarse resolution of 5x5 degrees.
RegridData = function(lat,lon,time,tas_annual){
  
  target_lat = seq(from=-87.5,by=5,to=87.5)
  target_lon = seq(from=-177.5,by=5,to=177.5)
  
  r = rast(tas_annual, 
           extent = ext(min(lon), max(lon), min(lat), max(lat)),
           crs = "epsg:4326") # Use WGS 84 as a common coordinate reference system
  
  
  target_raster = rast(ncol = 36, nrow = 72, 
                       extent = ext(r), 
                       crs = "epsg:4326")
  
  regridded_raster = resample(r, target_raster, method = "average")
  
  regridded_array = as.array(regridded_raster)
  return(regridded_array)
}
