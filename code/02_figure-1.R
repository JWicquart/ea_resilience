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

# 6. Subfigure D (DHW) ----

load("data/04_dhw/data_dhw_percent.RData")

plot_d <- data_dhw_percent %>% 
  filter(region == "EAS") %>%
  filter(dhw_type != "DHW = 0") %>% 
  mutate(dhw_type = as.factor(dhw_type)) %>% 
  ggplot(data = ., aes(x = date, y = freq, fill = dhw_type)) +
    geom_area(stat = "identity", position = "identity") +
    scale_y_continuous(limits = c(0, 110), breaks = c(0, 25, 50, 75, 100)) +
    scale_fill_manual(breaks = c("0 < DHW < 4", "4 <= DHW < 8", "DHW >= 8"), 
                      values = c("#2c82c9", "#fabe58", "#d64541"), name = NULL) +
    labs(x = "Year", y = "Sites (%)", title = "<b>D.</b> East Asian Seas") +
    theme_graph() +
    theme(legend.direction = "horizontal",
          legend.position = c(0.5, 0.925),
          plot.title = element_markdown(size = rel(1)),
          legend.background = element_blank()) +
  lims(x = c(as_date("1975-01-01"), as_date("2020-01-01")))

# 7. Subfigure E (DHW) ----

plot_e <- data_dhw_percent %>% 
  filter(region == "Other regions") %>%
  filter(dhw_type != "DHW = 0") %>% 
  mutate(dhw_type = as.factor(dhw_type)) %>% 
  ggplot(data = ., aes(x = date, y = freq, fill = dhw_type)) +
  geom_area(stat = "identity", position = "identity") +
  scale_y_continuous(limits = c(0, 110), breaks = c(0, 25, 50, 75, 100)) +
  scale_fill_manual(breaks = c("0 < DHW < 4", "4 <= DHW < 8", "DHW >= 8"), 
                    values = c("#2c82c9", "#fabe58", "#d64541"), name = NULL) +
  labs(x = "Year", y = "Sites (%)", title = "<b>E.</b> Other regions") +
  theme_graph() +
  theme(legend.direction = "horizontal",
        legend.position = c(0.5, 0.925),
        plot.title = element_markdown(size = rel(1)),
        legend.background = element_blank()) +
  lims(x = c(as_date("1975-01-01"), as_date("2020-01-01")))

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
