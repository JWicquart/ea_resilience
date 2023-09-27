# 1. Required packages ----

library(Rcpp)
library(tidyverse) # Core tidyverse packages
library(terra)
library(sf)
sf_use_s2(FALSE)
library(tidyterra)

# 2. Get NCDF files ----

ncdf_files <- list.files("../../pacific_2023/data/09_dhw", full.names = TRUE)

# 3. Load site coordinates ----

data_sites <- st_read("data/02_site-coords/ea_resilience_site-coords.shp") %>% 
  st_transform(crs = 4326)

# 4. Convert to terra format ----

data_sites_terra <- terra::vect(data_sites)

# Visual Check
#plot(rast(ncdf_files[1])$degree_heating_week)
#plot(data_sites, add = TRUE)

# 5. Create a function to extract DHW values for each site ----

extract_dhw_percent <- function(ncdf_i){
  
  ncdf <- rast(ncdf_i)$degree_heating_week
  
  crs(ncdf) <- "epsg:4326"
  
  results <- terra::extract(x = ncdf, y = data_sites_terra) %>% 
    mutate(date = lubridate::date(unique(time(ncdf))))
  
  return(results)
  
}

# 6. Map over the function ----

data_dhw_raw <- map_dfr(ncdf_files, ~extract_dhw_percent(.))

# 7. Calculate frequency of DHW (global) ----

data_dhw_global <- data_dhw_raw %>% 
  rename(dhw = degree_heating_week) %>% 
  filter(!(is.na(dhw))) %>% 
  mutate(dhw_type = case_when(dhw == 0 ~ "DHW = 0",
                              dhw > 0 & dhw < 4 ~ "0 < DHW < 4",
                              dhw >= 4 & dhw < 8 ~ "4 <= DHW < 8",
                              dhw >= 8 ~ "DHW >= 8")) %>% 
  group_by(date, dhw_type) %>% 
  dplyr::count(.) %>% 
  group_by(date) %>% 
  mutate(freq = n*100/sum(n)) %>% 
  ungroup() %>% 
  mutate(region = "All")

# 8. Calculate frequency of DHW (region) ----

data_dhw_regions <- data_dhw_raw %>% 
  rename(dhw = degree_heating_week,
         site_id = ID) %>% 
  left_join(data_sites %>% st_drop_geometry(), .) %>% 
  filter(!(is.na(dhw))) %>% 
  mutate(dhw_type = case_when(dhw == 0 ~ "DHW = 0",
                              dhw > 0 & dhw < 4 ~ "0 < DHW < 4",
                              dhw >= 4 & dhw < 8 ~ "4 <= DHW < 8",
                              dhw >= 8 ~ "DHW >= 8")) %>% 
  group_by(date, region, dhw_type) %>% 
  dplyr::count(.) %>% 
  group_by(date, region) %>% 
  mutate(freq = n*100/sum(n)) %>% 
  ungroup()

# 9. Combine data ----

data_dhw_percent <- bind_rows(data_dhw_global, data_dhw_regions)

# 10. Export the data ----

save(data_dhw_percent, file = "data/04_dhw/data_dhw_percent.RData")

# 11. Remove useless objects ----

rm(data_dhw_raw)
