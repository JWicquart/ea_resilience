# 1. Load packages ----

library(tidyverse)

# 2. Source functions ----

source("R/functions/graphical_par.R")
source("R/functions/theme_graph.R")

# 3. Percentage of coral reefs by GCRMN region ----

data_reefs <- tibble(GCRMN_region = c("Australia", "Brazil", "Caribbean", "East Asia", 
                                      "ETP", "Pacific", "PERSGA", "ROPME", "South Asia", "WIO"),
                     coral_reefs = c(16.1, 0.5, 10.2, 30.1, 0.3, 26.7, 5.2, 0.8, 4.2, 5.8))

A <- c("#67001f", "#b2182b", "#d6604d", "#fd8d3c", "#feb24c", "#4393c3", "#2166ac", "#053061", "#6a51a3", "#3f007d")

# 4. Hard coral cover data ----

# 4.1 Load and filter data --

data_regions <- read.csv("data/chapter_4/ModelledTrends.all.sum.csv") %>% 
  filter(Var == "Hard Coral Cover" & GCRMN_region != "Global") %>% 
  left_join(., data_reefs) %>% 
  mutate(GCRMN_region_portrait = paste0(GCRMN_region, " (", coral_reefs, "% of coral reefs)"),
         GCRMN_region_landscape = paste0(GCRMN_region, "\n", coral_reefs, "% of coral reefs"))

# 4.2 Make base figure --

plot_base <- ggplot(data = data_regions) +
  geom_ribbon(aes(x = Year, ymin = .lower_0.95, ymax = .upper_0.95, fill = GCRMN_region), 
              alpha = 0.25, show.legend = FALSE) +
  geom_ribbon(aes(x = Year, ymin = .lower_0.8, ymax = .upper_0.8, fill = GCRMN_region), 
              alpha = 0.5, show.legend = FALSE) +
  geom_line(aes(x = Year, y = value, col = GCRMN_region), 
            size = 1.2, show.legend = FALSE) +
  theme_graph() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 12)) +
  labs(x = "Year", y = "Hard coral cover (%)")

# 4.3 Export the figure for document (portrait mode) --

plot_base +
  facet_wrap(~GCRMN_region_portrait, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = palette_regions) +
  scale_color_manual(values = palette_regions)

ggsave("figs/chapter_4/figure-4.png", width = 6.5, height = 9, dpi = 600)

# 4.4 Export the figure for PowerPoint (landscape mode) --

plot_base +
  facet_wrap(~GCRMN_region_landscape, ncol = 5, scales = "free_y") +
  scale_fill_manual(values = palette_regions) +
  scale_color_manual(values = palette_regions) +
  geom_vline(xintercept = c(1998, 2010, 2016), linetype = "dashed", col = "red")

ggsave("figs/chapter_4/figure-4_ppt.png", width = 14, height = 5.5, dpi = 600)

# 5. Algae cover data ----

# 5.1 Load and filter data --

data_regions <- read.csv("data/chapter_4/ModelledTrends.all.sum.csv") %>% 
  filter(Var == "Algae" & GCRMN_region != "Global") %>% 
  left_join(., data_reefs) %>% 
  mutate(GCRMN_region_portrait = paste0(GCRMN_region, " (", coral_reefs, "% of coral reefs)"),
         GCRMN_region_landscape = paste0(GCRMN_region, "\n", coral_reefs, "% of coral reefs"))

# 5.2 Make base figure --

plot_base <- ggplot(data = data_regions) +
  geom_ribbon(aes(x = Year, ymin = .lower_0.95, ymax = .upper_0.95, fill = GCRMN_region), 
              alpha = 0.25, show.legend = FALSE) +
  geom_ribbon(aes(x = Year, ymin = .lower_0.8, ymax = .upper_0.8, fill = GCRMN_region), 
              alpha = 0.5, show.legend = FALSE) +
  geom_line(aes(x = Year, y = value, col = GCRMN_region), 
            size = 1.2, show.legend = FALSE) +
  theme_graph() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 12)) +
  labs(x = "Year", y = "Algae cover (%)")

# 5.3 Export the figure for document (portrait mode) --

plot_base +
  facet_wrap(~GCRMN_region_portrait, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = palette_regions) +
  scale_color_manual(values = palette_regions)

ggsave("figs/chapter_4/figure-5.png", width = 6.5, height = 9, dpi = 600)

# 5.4 Export the figure for PowerPoint (landscape mode) --

plot_base +
  facet_wrap(~GCRMN_region_landscape, ncol = 5, scales = "free_y") +
  scale_fill_manual(values = palette_regions) +
  scale_color_manual(values = palette_regions) +
  geom_vline(xintercept = c(1998, 2010, 2016), linetype = "dashed", col = "red")

ggsave("figs/chapter_4/figure-5_ppt.png", width = 14, height = 5.5, dpi = 600)
