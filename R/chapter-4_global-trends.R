# 1. Load packages ----

library(tidyverse)

# 2. Source functions ----

source("R/functions/graphical_par.R")
source("R/functions/theme_graph.R")

# 3. Hard coral cover data ----

# 3.1 Load and filter data --

data_global <- read.csv("data/chapter_4/ModelledTrends.all.sum.csv") %>% 
  filter(Var == "Hard Coral Cover" & GCRMN_region == "Global")

# 3.2 Make the figure --

ggplot(data = data_global) +
  geom_ribbon(aes(x = Year, ymin = .lower_0.95, ymax = .upper_0.95), 
              alpha = 0.25, fill = "#446CB3", show.legend = FALSE) +
  geom_ribbon(aes(x = Year, ymin = .lower_0.8, ymax = .upper_0.8), 
              alpha = 0.5, fill = "#446cb3", show.legend = FALSE) +
  geom_line(aes(x = Year, y = value), 
            size = 1.2, col = "#446CB3", show.legend = FALSE) +
  theme_graph() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 12)) +
  labs(x = "Year", y = "Hard coral cover (%)") +
  lims(x = c(1975, 2020))

# 3.3 Save the figure --

ggsave("figs/chapter_4/figure-2.png", width = 5, height = 3, dpi = 600)

# 4. Algae cover data ----

# 4.1 Load and filter data --

data_global <- read.csv("data/chapter_4/ModelledTrends.all.sum.csv") %>% 
  filter(Var == "Algae" & GCRMN_region == "Global")

# 4.2 Make the figure --

ggplot(data = data_global) +
  geom_ribbon(aes(x = Year, ymin = .lower_0.95, ymax = .upper_0.95), 
              alpha = 0.25, fill = "#446CB3", show.legend = FALSE) +
  geom_ribbon(aes(x = Year, ymin = .lower_0.8, ymax = .upper_0.8), 
              alpha = 0.5, fill = "#446cb3", show.legend = FALSE) +
  geom_line(aes(x = Year, y = value), 
            size = 1.2, col = "#446CB3", show.legend = FALSE) +
  theme_graph() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 12)) +
  labs(x = "Year", y = "Algae cover (%)") +
  lims(x = c(1975, 2020))

# 4.3 Save the figure --

ggsave("figs/chapter_4/figure-3.png", width = 5, height = 3, dpi = 600)
