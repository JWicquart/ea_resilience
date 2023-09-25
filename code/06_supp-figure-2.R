# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(ggtext)

# 2. Source functions ----

source("code/functions/graphical_par.R")
source("code/functions/theme_graph.R")
source("code/functions/transform_for_ribbon.R")

# 3. Load data ----

data_trend <- read.csv("data/ModelledTrends.all.sum.csv") %>% 
  filter(Var == "Hard Coral Cover" & GCRMN_region != "Global") %>% 
  mutate(GCRMN_region = str_replace_all(GCRMN_region, "East Asia", "East Asian Seas"))

data_reefs <- tibble(GCRMN_region = c("Australia", "Brazil", "Caribbean", "East Asian Seas", 
                                      "ETP", "Pacific", "PERSGA", "ROPME", "South Asia", "WIO"),
                     coral_reefs = c(16.1, 0.5, 10.2, 30.1, 0.3, 26.7, 5.2, 0.8, 4.2, 5.8),
                     color = palette_regions)

# 4. Make the plots ----

data_plots <- map(unique(data_trend$GCRMN_region),
                  ~transform_for_ribbon(data = data_trend, region = ., data_reefs = data_reefs))

# 5. Combine the plots ----

wrap_plots(data_plots) + plot_layout(ncol = 2)

# 6. Save the plot ----

ggsave("figs/supp-fig-2.png", height = 14, width = 7.5, dpi = 600)
