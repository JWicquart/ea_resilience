# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(terra)
library(sf)
sf_use_s2(FALSE)
library(future)
library(furrr) # For parallelization

plan(multisession, workers = 6) # Set parallelization with 6 cores

# 2. Get NCDF files ----

ncdf_files <- list.files("../../2022-10-12_pacific_2023/pacific_2023/data/09_dhw", full.names = TRUE)

# 3. Load site coordinates ----

data_sites <- st_read("data/02_site-coords/ea_resilience_site-coords.shp") %>% 
  st_transform(crs = 4326)

# Visual Check
#plot(rast(ncdf_files[1])$degree_heating_week)
#plot(data_sites, add = TRUE)

# 4. Create a function to extract DHW values for each site ----

extract_dhw_percent <- function(ncdf_i, sites){
  
  # 1. Extract DHW for each site ----
  
  ncdf <- terra::rast(ncdf_i)$degree_heating_week
  
  terra::crs(ncdf) <- "epsg:4326"
  
  data_sites <- sites
  
  data_sites_terra <- terra::vect(data_sites)

  data_dhw_raw <- terra::extract(x = ncdf, y = data_sites_terra)
  
  # 2. Calculate frequency of DHW ----
  
  # 2.1 Global --
  
  data_dhw_global <- data_dhw_raw %>% 
    dplyr::rename(dhw = degree_heating_week) %>% 
    dplyr::filter(!(is.na(dhw))) %>% 
    dplyr::mutate(dhw_type = case_when(dhw == 0 ~ "DHW = 0",
                                       dhw > 0 & dhw < 4 ~ "0 < DHW < 4",
                                       dhw >= 4 & dhw < 8 ~ "4 <= DHW < 8",
                                       dhw >= 8 ~ "DHW >= 8")) %>% 
    dplyr::group_by(dhw_type) %>% 
    dplyr::count(.)  %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(freq = n*100/sum(n)) %>% 
    dplyr::mutate(region = "All")
  
  # 2.2 All regions except EAS --
  
  data_dhw_not_eas <- data_dhw_raw %>% 
    dplyr::rename(dhw = degree_heating_week,
           site_id = ID) %>% 
    dplyr::left_join(data_sites %>% st_drop_geometry(), .) %>% 
    dplyr::filter(region != "EAS") %>% 
    dplyr::filter(!(is.na(dhw))) %>% 
    dplyr::mutate(dhw_type = case_when(dhw == 0 ~ "DHW = 0",
                                       dhw > 0 & dhw < 4 ~ "0 < DHW < 4",
                                       dhw >= 4 & dhw < 8 ~ "4 <= DHW < 8",
                                       dhw >= 8 ~ "DHW >= 8")) %>% 
    dplyr::group_by(dhw_type) %>% 
    dplyr:::count(.)  %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(freq = n*100/sum(n)) %>% 
    dplyr::mutate(region = "Other regions")
  
  # 2.3 For each region --
  
  data_dhw_regions <- data_dhw_raw %>% 
    dplyr::rename(dhw = degree_heating_week,
           site_id = ID) %>% 
    dplyr::left_join(data_sites %>% st_drop_geometry(), .) %>% 
    dplyr::filter(!(is.na(dhw))) %>% 
    dplyr::mutate(dhw_type = case_when(dhw == 0 ~ "DHW = 0",
                                       dhw > 0 & dhw < 4 ~ "0 < DHW < 4",
                                       dhw >= 4 & dhw < 8 ~ "4 <= DHW < 8",
                                       dhw >= 8 ~ "DHW >= 8")) %>% 
    dplyr::group_by(region, dhw_type) %>% 
    dplyr::count(.) %>% 
    dplyr::group_by(region) %>% 
    dplyr::mutate(freq = n*100/sum(n)) %>% 
    dplyr::ungroup()
  
  # 3. Combine data ----
  
  results <- bind_rows(data_dhw_global, data_dhw_not_eas) %>%
    bind_rows(., data_dhw_regions) %>% 
    mutate(date = lubridate::date(unique(time(ncdf))))
  
  # 4. Return results ----
  
  return(results)
  
}

# 5. Map over the function ----

data_dhw_percent <- future_map_dfr(ncdf_files, ~extract_dhw_percent(ncdf_i = ., sites = data_sites))

# 6. Export the data ----

save(data_dhw_percent, file = "data/04_dhw/data_dhw_percent.RData")
