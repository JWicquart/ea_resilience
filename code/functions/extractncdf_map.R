extractncdf_map <- function(data, coordinates){
  
  ncdf_i <- brick(data, varname = "analysed_sst")
  
  values_sst_i <- extract(ncdf_i, coordinates, method = "bilinear") %>% 
    tibble(timeseries_id = row_number(coordinates@data[,1]),
           GCRMN_regions = coordinates@data[,1],
           lat = coordinates@coords[,"lat"],
           long = coordinates@coords[,"long"],
           date = as.Date(ncdf_i@z[[1]]),
           sst = .)
  
  return(values_sst_i)
  
}