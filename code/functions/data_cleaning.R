data_cleaning <- function(data){
  
  crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  
  load("data/00_shapefiles/gcrmn_regions.RData")
  
  cleaned_data <- data %>%  
    filter(!(DatasetID %in% c("XLCA1", "XLCA2", "XLCA3", "XLCA4", "XLCA5",
                              "PACN1.1", "PACN1.2", "PACN1.3", "PACN1.4",
                              "TIAH1", "RFCK1"))) %>% # Remove datasets unused by Murray for the analyses
    # Sites with incorrect position (not corrected during QAQC)
    filter(!(Longitude %in% c(33.50138889, 52.7500, 50.71666667))) %>% 
    filter(!(Site %in% c("34.26.62E.28.51.79S", "34.41.08E.27.31.66S", "NSIslandsSouth", 
                         "29N34E4", "34.4.4.97E.29.7.39.89N", "34.26.013E.28.26.151N", "34.18.5E27.54.8N"))) %>% 
    mutate(Date = as.Date(Date)) %>% 
    select(Latitude, Longitude) %>% 
    drop_na(Latitude, Longitude) %>% 
    distinct() %>% 
    st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326) %>% 
    # Add gcrmn_region using a spatial join
    st_intersection(., data_gcrmn_regions) %>% 
    # Remove point falling outside gcrmn_regions polygon
    drop_na(gcrmn_region) %>% 
    st_transform(crs = crs_selected)
  
  return(cleaned_data)
  
}