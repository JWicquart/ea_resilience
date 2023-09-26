# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(ggtext)
library(sf)
sf_use_s2(FALSE) # Switch from S2 to GEOS

# 2. Source functions ----

source("code/functions/graphical_par.R")
source("code/functions/theme_graph.R")

# 3. Load and transform benthic cover synthetic dataset ----

# 3.1 Define the CRS --

crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# 3.2 GCRMN regions --

load("data/gcrmn_regions.RData")

# 3.3 Benthic cover synthetic dataset --

gcrmn_path <- "C:/Users/jwicquart/Desktop/Recherche/03_projects/2021-07-21_monitoring_reefs/monitoring_reefs/data/"

data_benthos <- read.csv2(paste0(gcrmn_path, "03-merge_all_all_all_benthos_NA.csv"), stringsAsFactors = TRUE)

# 3.4 Remove non used data for the analyses --

data_benthos_sites <- data_benthos %>% 
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
  st_transform(crs = crs_selected) %>% 
  st_transform(crs = 4326)

# 3.5 Join with main data --

data_benthos <- tibble(Longitude = st_coordinates(data_benthos_sites)[,1],
                       Latitude = st_coordinates(data_benthos_sites)[,2]) %>% 
  left_join(., data_benthos)

# 4. Make the plot of number of surveys per year ----

plot_a <- data_benthos %>% 
  select(Latitude, Longitude, Year, Date) %>% 
  distinct() %>% 
  group_by(Year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = (n*100)/sum(n)) %>% 
  ggplot(data = ., aes(x = Year, y = perc)) +
    geom_bar(stat = "identity", fill = col_color_graph) +
    theme_graph() +
    labs(x = "Year", y = "Percentage of surveys", title = "A") +
    theme(plot.title = element_markdown(size = rel(1)))


# 5. Make the plot of number of surveys per depth ----

plot_b <- data_benthos %>% 
  select(Latitude, Longitude, Year, Date, Depth) %>% 
  distinct() %>% 
  ggplot(data = ., aes(x = Depth)) +
    geom_histogram(binwidth = 1, aes(y = stat(width*density*100)),
                   color = "white", fill = col_color_graph) +
    theme_graph() +
    labs(x = "Depth", y = "Percentage of surveys", title = "B") +
    lims(x = c(0, 40)) +
    theme(plot.title = element_markdown(size = rel(1)))

# 6. Combine the plots ----

plot_a + plot_b

# 7. Save the plot ----

ggsave("figs/supp-fig-3.png", height = 4, width = 9, dpi = 600)
