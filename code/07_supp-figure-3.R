# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(ggtext)
library(sf)
sf_use_s2(FALSE) # Switch from S2 to GEOS

# 2. Source functions ----

source("code/functions/graphical_par.R")
source("code/functions/theme_graph.R")
source("code/functions/data_cleaning.R")

# 3. Load and transform benthic cover synthetic dataset ----

# 3.1 Benthic cover synthetic dataset --

data_benthos <- read.csv2("data/01_benthic-data/03-merge_all_all_all_benthos_NA.csv", stringsAsFactors = TRUE)

# 3.2 Remove non used data for the analyses --

data_benthos_sites <- data_benthos %>% 
  data_cleaning(data = .) %>% 
  st_transform(crs = 4326)

# 3.3 Join with main data --

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
    geom_histogram(binwidth = 1, aes(y = after_stat(width * density * 100)),
                   color = "white", fill = col_color_graph) +
    theme_graph() +
    labs(x = "Depth (m)", y = "Percentage of surveys", title = "B") +
    lims(x = c(0, 40)) +
    theme(plot.title = element_markdown(size = rel(1)))

# 6. Combine the plots ----

plot_a + plot_b

# 7. Save the plot ----

ggsave("figs/supp-fig-3.png", height = 4, width = 9, dpi = 600)
