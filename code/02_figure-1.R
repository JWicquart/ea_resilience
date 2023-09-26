# 1. Load packages ----

library(tidyverse)
library(magrittr)
library(sf)
sf_use_s2(FALSE) # Switch from S2 to GEOS
library(patchwork)
library(ggtext)

# 2. Source functions ----

source("code/functions/graphical_par.R")
source("code/functions/theme_map.R")
source("code/functions/theme_graph.R")
source("code/functions/transform_for_ribbon.R")
source("code/functions/data_cleaning.R")

# 3. Subfigure A (map) ----

crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

## 3.1 Load shapefile ----

data_map <- read_sf("data/00_shapefiles/ne_10m_land/ne_10m_land.shp") %>% 
  st_transform(crs = 4326)

## 3.2 Define the offset ----

correction_offset <- 180 - 160 # Here 160 is the same value than +lon_0 from crs_selected

## 3.3 Define a long and slim polygon that overlaps the meridian line ----

correction_polygon <- st_polygon(x = list(rbind(c(-0.0001 - correction_offset, 90),
                                                c(0 - correction_offset, 90),
                                                c(0 - correction_offset, -90),
                                                c(-0.0001 - correction_offset, -90),
                                                c(-0.0001 - correction_offset, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

## 3.4 Modify data_map to remove overlapping portions using correction_polygon ----

data_map <- data_map %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

## 3.5 Load country boundaries data ----

data_countries <- read_sf("data/00_shapefiles/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_transform(crs = crs_selected)

## 3.6 Load and transform benthic cover synthetic dataset ----

# GCRMN regions --

load("data/00_shapefiles/gcrmn_regions.RData")

# Benthic cover synthetic dataset --

data_benthos <- read.csv2("data/01_benthic-data/03-merge_all_all_all_benthos_NA.csv", stringsAsFactors = TRUE) %>% 
  data_cleaning(data = .) %>% 
  mutate(color = if_else(gcrmn_region == "EAS", "#d24d57", "#446CB3"))

## 3.7 Transform CRS ----

data_gcrmn_regions <- data_gcrmn_regions %>% 
  st_transform(crs = crs_selected) %>% 
  mutate(color = if_else(gcrmn_region == "EAS", "#d24d57", "#446CB3"))

data_gcrmn_regions[7,] %<>% # Pacific is row number 7; note the special pipe from magrittr
  st_buffer(1000) # To join Pacific polygons

## 3.8 Create the tropics ----

data_tropics <- tibble(long = c(-180, 180, -180, 180, -180, 180), 
                       lat = c(0, 0, 23.43656, 23.43656, -23.43656, -23.43656), 
                       tropic = c("Equator", "Equator", "Tropic of Cancer", "Tropic of Cancer",
                                  "Tropic of Capricorne", "Tropic of Capricorne")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  group_by(tropic) %>% 
  summarise() %>% 
  st_cast("LINESTRING") %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

data_tropics_no_overlap <- st_intersection(data_tropics, data_gcrmn_regions)

data_tropics_no_overlap$var <- "true"

data_tropics_no_overlap <- st_difference(data_tropics, st_union(st_geometry(data_tropics_no_overlap)))

## 3.9 Make the map ----

plot_a <- ggplot() +
  # Tropics
  geom_sf(data = data_tropics_no_overlap, linetype = "dashed", color = "#363737", linewidth = 0.25) +
  # GCRMN region
  geom_sf(data = data_gcrmn_regions, aes(fill = color), alpha = 0.3) +
  # Site of benthic cover data
  geom_sf(data = data_benthos, aes(color = color), size = 0.75) +
  # Background map
  geom_sf(data = data_map, size = 0.25) +
  # Country boundaries
  geom_sf(data = data_countries, size = 0.2) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  coord_sf(ylim = c(-5000000, 5000000), expand = FALSE) +
  theme_map() +
  labs(title = "<b>A</b>") +
  scale_color_identity() +
  scale_fill_identity() +
  theme(plot.title = element_markdown(size = rel(1)))

# 4. Subfigure B (HCC) ----

## 4.1 Load and filter data ----

data_trend <- read.csv("data/03_modelled-data/ModelledTrends.all.sum.csv") %>% 
  filter(Var == "Hard Coral Cover" & GCRMN_region == "East Asia")

## 4.2 Make the figure ----

plot_b <- transform_for_ribbon(data = data_trend, region = "East Asia",
                               title = FALSE, ribbon_color = "#d24d57",
                               title_name = "<b>B.</b> East Asian Seas") +
  geom_vline(xintercept = c(1998.6, 2010.6, 2016.0), linetype = "dashed") +
  lims(x = c(1975, 2020))

# 5. Subfigure C (HCC) ----

## 5.1 Load and filter data ----

data_trend <- readRDS("data/03_modelled-data/GlobalTrend_sans_EastAsia.RData")$data %>% 
  mutate(across(c("value", ".lower_0.8", ".lower_0.95", ".upper_0.8", ".upper_0.95"), ~.x*100)) %>% 
  mutate(GCRMN_region = "Other regions")

## 5.2 Make the figure ----

plot_c <- transform_for_ribbon(data = data_trend, region = "Other regions",
                               title = FALSE, ribbon_color = "#446CB3",
                               title_name = "<b>C.</b> Other regions") +
  geom_vline(xintercept = c(1998.6, 2010.6, 2016.0), linetype = "dashed") +
  lims(x = c(1975, 2020))

# 6. Subfigure D (SSTa) ----

## 6.1 Load and transform data ----

# SST anomaly data --

load("data/04_sst/sst_anomaly.RData")

data_sst_anom <- data_sst_anom %>% 
  drop_na(sst_anomaly_mean) %>% 
  filter(date < as.Date("2020-01-01")) %>% 
  mutate(sst_anomaly_type = ifelse(sst_anomaly_mean > 0,"#d24d57", "#446CB3"))

# Add number of sites per GCRMN region --

load("data/site_coordinates.RData")

data_sst_anom <- data_benthos %>% 
  st_drop_geometry() %>% 
  group_by(gcrmn_region) %>% 
  count() %>% 
  mutate(n = as.character(n),
         n = ifelse(nchar(n) == 4, paste0(substr(n, 1, 1), ",", substr(n, 2, 4)), n)) %>% 
  ungroup() %>% 
  left_join(data_sst_anom, .) %>% 
  mutate(gcrmn_region = paste0(gcrmn_region, " (n = ", n, ")"))

## 6.2 Make the plot ----

plot_d <- data_sst_anom %>% 
  filter(gcrmn_region == "East Asia (n = 2,877)") %>% 
  ggplot(data = ., aes(x = date, y = sst_anomaly_mean)) +
  geom_vline(xintercept = c(as.Date("1998-06-01"), 
                            as.Date("2010-06-01"), 
                            as.Date("2016-01-01")), linetype = "dashed") +
  geom_ribbon(aes(ymin = 0, ymax = sst_anomaly_mean), fill = "#d24d57", alpha = 0.5) +
  geom_path(linewidth = 0.4) +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 12)) +
  labs(x = "Year", y = "SST anomaly (°C)", title = "<b>D.</b> East Asian Seas") +
  lims(y = c(-0.5, 0.7), x = c(as.Date("1975-01-01"), as.Date("2020-01-01"))) +
  scale_fill_identity() +
  theme_graph() +
  theme(plot.title = element_markdown(size = rel(1)))

# 7. Subfigure E (SSTa) ----

plot_e <- data_sst_anom %>% 
  filter(gcrmn_region != "East Asia (n = 2,877)") %>% 
  group_by(date) %>% 
  summarise(sst_anomaly_mean = mean(sst_anomaly_mean)) %>% 
  ungroup() %>% 
  mutate(sst_anomaly_type = ifelse(sst_anomaly_mean > 0,"#d24d57", "#446CB3")) %>% 
  ggplot(data = ., aes(x = date, y = sst_anomaly_mean)) +
  geom_vline(xintercept = c(as.Date("1998-06-01"), 
                            as.Date("2010-06-01"), 
                            as.Date("2016-01-01")), linetype = "dashed") +
  geom_ribbon(aes(ymin = 0, ymax = sst_anomaly_mean), fill = "#446CB3", alpha = 0.5) +
  geom_path(linewidth = 0.4) +
  labs(x = "Year", 
       y = "SST anomaly (°C)", 
       title = "<b>E.</b> Other regions") +
  lims(y = c(-0.5, 0.7), x = c(as.Date("1975-01-01"), as.Date("2020-01-01"))) +
  scale_fill_identity() +
  theme_graph() +
  theme(plot.title = element_markdown(size = rel(1)))

# 8. Combine the plots ----

## 8.1 Create the layout ----

layout <- "
AA
BC
DE
"

## 8.2 Combine plots ----

plot_a + plot_b + plot_c + plot_d + plot_e +
  plot_layout(design = layout, heights = c(0.55, 0.45, 0.45))

## 8.3 Save the plot ----

ggsave("figs/figure-1.png", height = 8, width = 9, dpi = 600)
