# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(ggtext)

# 2. Source functions ----

source("code/functions/graphical_par.R")
source("code/functions/theme_graph.R")
source("code/functions/transform_for_ribbon.R")

# 3. Global trend (HCC) ----

# 3.1 Load data --

data_trend <- read.csv("data/ModelledTrends.all.sum.csv") %>% 
  filter(Var == "Hard Coral Cover" & GCRMN_region == "Global")

# 3.2 Make the figure --

plot_a <- transform_for_ribbon(data = data_trend,
                               region = "Global", title = FALSE,
                               ribbon_color = "#446CB3", title_name = "<b>A</b>") +
  geom_vline(xintercept = c(1998.6, 2010.6, 2016.0), linetype = "dashed") +
  lims(x = c(1975, 2020))

# 4. Global trend (SSTa) ----

# 4.1 Load data --

load("data/sst_anomaly.RData")

# 4.2 Make the figure --

plot_b <- data_sst_anom %>% 
  group_by(date) %>% 
  summarise(sst_anomaly_mean = mean(sst_anomaly_mean)) %>% 
  ungroup() %>% 
  mutate(sst_anomaly_type = ifelse(sst_anomaly_mean > 0,"#ec644b", "#59abe3")) %>% 
  ggplot(data = ., aes(x = date, y = sst_anomaly_mean)) +
  geom_vline(xintercept = c(as.Date("1998-06-01"), 
                            as.Date("2010-06-01"), 
                            as.Date("2016-01-01")), linetype = "dashed") +
  geom_ribbon(aes(ymin = 0, ymax = sst_anomaly_mean), fill = "#446CB3", alpha = 0.5) +
  geom_path(linewidth = 0.5) +
  theme_graph() +
  labs(x = "Year", 
       y = "SST anomaly (°C)", 
       title = "<b>B</b>") +
  lims(y = c(-0.5, 0.7), x = c(as.Date("1975-01-01"), as.Date("2020-01-01"))) +
  scale_fill_identity() +
  theme(plot.title = element_markdown(size = rel(1)))

# 5. Combine plots ----

plot_a + plot_b + plot_layout(ncol = 1)

# 6. Save the plot ----

ggsave("figs/supp-fig-1.png", height = 7, width = 6, dpi = 600)
