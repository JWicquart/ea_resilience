# 1. Load packages ----

library(tidyverse)
library(patchwork)
library(tidytext)

# 2. Source functions ----

source("R/functions/graphical_par.R")
source("R/functions/theme_graph.R")

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
  mutate(bound = "Overall") %>% 
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
  mutate(bound = str_replace_all(bound, c("MBE1998" = "1998 mass bleaching event",
                                          "MBE2010" = "2010 mass bleaching event",
                                          "MBE2016" = "2016 mass bleaching event")))

# 3.3 Combine data --

data_combined <- bind_rows(data_events, data_overall) %>% 
  mutate(bound = fct_relevel(bound, c("Overall", 
                                      "1998 mass bleaching event", 
                                      "2010 mass bleaching event", 
                                      "2016 mass bleaching event")))


data_combined %>% 
  filter(GCRMN_region %in% c("Global", "East Asia")) %>% 
  ggplot(data = ., aes(x = GCRMN_region, y = mean, ymin = lower, ymax = upper)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(shape = 21, fill = "red", size = 1) +
    coord_flip() +
    labs(y = "Difference", x = NULL) +
    facet_wrap(~bound, nrow = 1) +
    theme_graph()

data_combined %>% 
  mutate(GCRMN_region = reorder_within(GCRMN_region, mean, bound)) %>% 
  ggplot(data = ., aes(x = GCRMN_region, y = mean, ymin = lower, ymax = upper, group = bound)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(shape = 21, size = 0.75, aes(fill = if_else(str_detect(GCRMN_region, "East Asia") == TRUE, "#59abe3", "#ec644b")), show.legend = FALSE) +
    coord_flip() +
    scale_fill_identity() +
    labs(y = "Absolute difference in HCC", x = NULL) +
    facet_wrap(~bound, scales = "free_y") +
  scale_x_reordered() +
    theme_graph()


ggsave("figs/figure-3.png", height = 8, width = 8, dpi = 600)


# TEST LM

data_trend <- read.csv("data/ModelledTrends.all.sum.csv") %>% 
  filter(Var == "Hard Coral Cover") %>% 
  filter(Year %in% 1996:2019) %>% 
  nest(data = -GCRMN_region) %>% 
  mutate(model = map(data, ~lm(value ~ Year, data = .)),
         model = map(model, broom::tidy)) %>% 
  unnest(model) %>% 
  filter(term == "Year")
