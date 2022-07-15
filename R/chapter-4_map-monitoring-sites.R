# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE) # Switch from S2 to GEOS

# 2. Source functions ----

source("R/functions/graphical_par.R")
source("R/functions/theme_map.R")

# 3. Define the CRS ----

crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# 4. Load background maps and change its CRS ----

# 4.1 Load shapefile --

data_map <- read_sf("data/00_natural-earth-data/ne_10m_land/ne_10m_land.shp") %>% 
  st_transform(crs = 4326)

# 4.2 Define the offset --

correction_offset <- 180 - 160 # Here 160 is the same value than +lon_0 from crs_selected

# 4.3 Define a long and slim polygon that overlaps the meridian line --

correction_polygon <- st_polygon(x = list(rbind(c(-0.0001 - correction_offset, 90),
                                                c(0 - correction_offset, 90),
                                                c(0 - correction_offset, -90),
                                                c(-0.0001 - correction_offset, -90),
                                                c(-0.0001 - correction_offset, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# 4.4 Modify data_map to remove overlapping portions using correction_polygon --

data_map <- data_map %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

# 5. Load country boundaries data ----

data_countries <- read_sf("data/00_natural-earth-data/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_transform(crs = crs_selected)

# 6. Load and transform benthic cover synthetic dataset ----

# 6.1 GCRMN regions --

load("data/gcrmn_regions.RData")

# 6.2 Benthic cover synthetic dataset --

gcrmn_path <- "C:/Users/jwicquart/Desktop/Recherche/Projets/2019-07-08 - Global Coral Reef Monitoring Network/GCRMN_2020/data/"

data_benthos <- read.csv2(paste0(gcrmn_path, "03-merge_all_all_all_benthos_NA.csv"), stringsAsFactors = TRUE) %>% 
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

# 7. Make the map ----

ggplot() +
  # Tropics
  geom_hline(yintercept = c(2608958.17028999, 0, -2608958.17028999), linetype = "dashed", col = "black", size = 0.2) +
  # Site of benthic cover data
  geom_sf(data = data_benthos, aes(color = gcrmn_region)) +
  # Background map
  geom_sf(data = data_map, size = 0.25) +
  # Country boundaries
  geom_sf(data = data_countries, size = 0.2) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  coord_sf(ylim = c(-5000000, 5000000), expand = FALSE) +
  theme_map() +
  scale_color_manual(values = palette_regions)

# 8. Save the figure ----

ggsave("figs/figure-1.png", width = 7, height = 3, dpi = 600)

# 9. Save the sf object to extract SST anomalies ----

save(data_benthos, file = "data/site_coordinates.RData")
