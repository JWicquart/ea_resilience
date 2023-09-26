# 1. Load packages ----

library(tidyverse)
library(patchwork)
library(tidytext)
library(ggtext)

# 2. Source functions ----

source("code/functions/graphical_par.R")
source("code/functions/theme_graph.R")

# 3. Transform data ----

# 3.1 Overall trend --

data_overall <- read.csv("data/ModelledTrends.all.sum.csv") %>% 
  filter(Var == "Hard Coral Cover") %>% 
  filter(Year %in% 1996:2019) %>% 
  rename(mean = value,
         upper = .upper_0.8,
         lower = .lower_0.8) %>% 
  mutate(bound = case_when(Year %in% 1996:1998 ~ "before",
                           Year %in% 2017:2019 ~ "after")) %>% 
  drop_na(bound) %>% 
  group_by(GCRMN_region, bound) %>% 
  summarise(mean = mean(mean),
            lower = mean(lower),
            upper = mean(upper)) %>%
  ungroup() %>% 
  arrange(GCRMN_region, bound) %>% 
  mutate_at(vars(mean, lower, upper), ~if_else(str_detect(bound, "before") == TRUE, -.x, .x)) %>% 
  group_by(GCRMN_region) %>% 
  summarise_at(vars(mean, lower, upper), ~sum(.x)) %>% 
  ungroup() %>% 
  mutate(bound = "<b>A.</b> Overall (1996 to 2019)") %>% 
  mutate(GCRMN_region = fct_reorder(GCRMN_region, mean, .desc = TRUE))

# 3.2 Mass bleaching events --

data_events <- read.csv("data/ModelledTrends.all.sum.csv") %>% 
  filter(Var == "Hard Coral Cover") %>% 
  rename(mean = value,
         upper = .upper_0.8,
         lower = .lower_0.8) %>% 
  mutate(bound_year = case_when(Year %in% 1996:1998 ~ "MBE1998_before",
                                Year %in% 1999:2001 ~ "MBE1998_after",
                                Year %in% 2008:2010 ~ "MBE2010_before",
                                Year %in% 2011:2013 ~ "MBE2010_after",
                                Year %in% 2014:2016 ~ "MBE2016_before",
                                Year %in% 2017:2019 ~ "MBE2016_after")) %>% 
  group_by(GCRMN_region, bound_year) %>% 
  summarise(mean = mean(mean),
            lower = mean(lower),
            upper = mean(upper)) %>%
  ungroup() %>% 
  drop_na(bound_year) %>% 
  arrange(GCRMN_region, bound_year) %>% 
  mutate(bound = str_sub(bound_year, 1, 7)) %>%
  mutate_at(vars(mean, lower, upper), ~if_else(str_detect(bound_year, "before") == TRUE, -.x, .x)) %>% 
  group_by(GCRMN_region, bound) %>% 
  summarise_at(vars(mean, lower, upper), ~sum(.x)) %>% 
  ungroup() %>% 
  mutate(bound = str_replace_all(bound, c("MBE1998" = "<b>B.</b> 1998 mass bleaching event",
                                          "MBE2010" = "<b>C.</b> 2010 mass bleaching event",
                                          "MBE2016" = "<b>D.</b> 2016 mass bleaching event")))

# 3.3 Combine data --

data_combined <- bind_rows(data_events, data_overall) %>% 
  mutate(bound = fct_relevel(bound, c("<b>A.</b> Overall (1996 to 2019)", 
                                      "<b>B.</b> 1998 mass bleaching event", 
                                      "<b>C.</b> 2010 mass bleaching event", 
                                      "<b>D.</b> 2016 mass bleaching event")),
         GCRMN_region = str_replace_all(GCRMN_region, c("Global" = "**Global**",
                                                        "East Asia" = "<span style='color:#ec644b'>**East Asian Seas**</span>")),
         GCRMN_region = reorder_within(GCRMN_region, mean, bound))

# 4. Make the plot ----

ggplot(data = data_combined, aes(x = GCRMN_region, y = mean, group = bound)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_segment(aes(yend = 0, xend = GCRMN_region)) +
  geom_point(shape = 21, size = 3, color = "white",
             aes(fill = if_else(str_detect(GCRMN_region, "East Asia") == TRUE, "#d24d57", "#446CB3")),
             show.legend = FALSE) +
  coord_flip() +
  scale_fill_identity() +
  labs(y = "Absolute difference in HCC (%)", x = NULL) +
  facet_wrap(~bound, scales = "free_y") +
  scale_x_reordered() +
  theme_graph() +
  theme(axis.text.y = element_markdown(),
        strip.background = element_blank(),
        strip.text = element_markdown(hjust = -0.05, size = 11))

# 5. Save the plot ----

ggsave("figs/figure-2.png", height = 6, width = 8, dpi = 600)
