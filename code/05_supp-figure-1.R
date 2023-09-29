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

data_trend <- read.csv("data/03_modelled-data/ModelledTrends.all.sum.csv") %>% 
  filter(Var == "Hard Coral Cover" & GCRMN_region == "Global")

# 3.2 Make the figure --

plot_a <- transform_for_ribbon(data = data_trend,
                               region = "Global", title = FALSE,
                               ribbon_color = "#446CB3", title_name = "<b>A</b>") +
  geom_vline(xintercept = c(1998.6, 2010.6, 2016.0), linetype = "dashed") +
  lims(x = c(1975, 2020))

# 4. Global trend (DHW) ----

load("data/04_dhw/data_dhw_percent.RData")

plot_b <- data_dhw_percent %>% 
  filter(region == "All") %>%
  filter(dhw_type != "DHW = 0") %>% 
  mutate(dhw_type = as.factor(dhw_type)) %>% 
  ggplot(data = ., aes(x = date, y = freq, fill = dhw_type)) +
  geom_area(stat = "identity", position = "identity") +
  scale_y_continuous(limits = c(0, 110), breaks = c(0, 25, 50, 75, 100)) +
  scale_fill_manual(breaks = c("0 < DHW < 4", "4 <= DHW < 8", "DHW >= 8"), 
                    values = c("#2c82c9", "#fabe58", "#d64541"), name = NULL) +
  labs(x = "Year", y = "Sites (%)", title = "<b>B</b>") +
  theme_graph() +
  theme(legend.direction = "horizontal",
        legend.position = c(0.5, 0.925),
        plot.title = element_markdown(size = rel(1)),
        legend.background = element_blank()) +
  lims(x = c(as_date("1975-01-01"), as_date("2020-01-01")))

# 5. Combine plots ----

plot_a + plot_b + plot_layout(ncol = 1)

# 6. Save the plot ----

ggsave("figs/supp-fig-1.png", height = 7, width = 6, dpi = 600)
