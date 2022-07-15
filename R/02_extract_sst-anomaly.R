# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(lubridate)
library(sf)
library(ncdf4)
library(raster)
library(sp)
library(future)
library(furrr) # For parallelization
library(RcppRoll)

plan(multisession, workers = 6) # Set parallelization with 6 cores

# 2. Required functions ----

source("R/functions/graphical_par.R")
source("R/functions/theme_graph.R")
source("R/functions/extractncdf_map.R") # NetCDF SST extraction

# 3. List of NetCDF4 files ----

path_ncdf <- "C:/Users/jwicquart/Desktop/Recherche/Projets/2020-07-30 - Resilience and disturbance characteristics/disturbance/data/011_sst_raw/"

ncdf_files <- list.files(path = path_ncdf, pattern = "\\.nc$", full.names = TRUE)

# 4. Check if files are missing ----

real_files_list <- str_remove_all(str_split_fixed(ncdf_files, "_", n = 5)[,5], "\\.nc")
  
theoric_files_list <- str_remove_all(seq(as.Date("1985-01-01"), as.Date("2020-12-31"), by = "days"), "-")

setdiff(theoric_files_list, real_files_list)

rm(theoric_files_list, real_files_list)

# 5. File of site coordinates ----

load("data/site_coordinates.RData")

data_benthos <- data_benthos %>% 
  st_transform(crs = 4326)

site_coordinates <- tibble(gcrmn_region = data_benthos$gcrmn_region,
                           long = st_coordinates(data_benthos)[,1],
                           lat = st_coordinates(data_benthos)[,2])

site_coordinates <- SpatialPointsDataFrame(
  # Coordinates
  coords = site_coordinates[, c("long", "lat")],
  # Data
  data = as.data.frame(site_coordinates[, "gcrmn_region"]),
  # Projection (crs)
  proj4string = CRS("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

# 6. Extract SST values ----

data_sst <- future_map_dfr(ncdf_files, ~extractncdf_map(data = ., coordinates = site_coordinates))

# 7. Calculate SST anomaly ----

data_sst_anom <- data_sst %>% 
  group_by(gcrmn_region, date) %>% 
  summarise(sst = mean(sst, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(gcrmn_region) %>% 
  mutate(mean_sst = mean(sst, na.rm = TRUE),
         sst_anomaly = sst - mean_sst,
         sst_anomaly_mean = roll_mean(x = sst_anomaly, n = 365, align = "center", fill = NA)) %>% 
  ungroup() %>% 
  mutate(date = as.Date(date))

# 8. Save the data ----

save(data_sst_anom, file = "data/sst_anomaly.RData")
