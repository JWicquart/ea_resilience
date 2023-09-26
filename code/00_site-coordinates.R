# 1. Load packages ----

library(tidyverse)
library(magrittr)
library(sf)
sf_use_s2(FALSE) # Switch from S2 to GEOS

# 2. Source functions ----

source("code/functions/data_cleaning.R")

# 3. Extract and export site coordinates ----

data_benthos <- read.csv2("data/01_benthic-data/03-merge_all_all_all_benthos_NA.csv", stringsAsFactors = TRUE) %>% 
  data_cleaning(data = .) %>% 
  st_transform(crs = 4326) %>% 
  mutate(site_id = row_number(.)) %>% 
  select(-gcrmn_region) %>% 
  st_write(., dsn = "data/02_site-coords/ea_resilience_site-coords.shp", delete_dsn = TRUE)
