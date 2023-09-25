before_after_interval_3 <- function(event, region){
  
  data_years_i <- data_years %>%
    filter(event == event)
    
  # 1. Generate a normal distribution
  
  # 1.1 For year_before
  
  data <- data_overall %>% 
    filter(Year %in% unique(data_years_i$year_before) & GCRMN_region == region) %>% 
    summarise_at(c("value", ".lower_0.8", ".upper_0.8"), mean)
  
  sd <- mean(data$value-data$.lower_0.8, data$.upper_0.8-data$value)
  
  dist_before <- rnorm(1000, mean = data$value, sd = sd)
  
  # 1.2 For year_after
  
  data <- data_overall %>% 
    filter(Year %in% unique(data_years_i$year_after) & GCRMN_region == region) %>% 
    summarise_at(c("value", ".lower_0.8", ".upper_0.8"), mean)
  
  sd <- mean(data$value-data$.lower_0.8, data$.upper_0.8-data$value)
  
  dist_after <- rnorm(1000, mean = data$value, sd = sd)
  
  # 2. Get difference based on random sampling
  
  vector <- numeric(0)
  
  for (i in 1:1000){
    
    vector[i] <- (sample(dist_after, size = 1) - sample(dist_before, size = 1))
    
  }
  
  # 3. Calculate CI
  
  data <- tibble(event = event,
                 region = region,
                 mean = mean(vector),
                 ic95_lower = quantile(vector, probs = 0.025),
                 ic95_upper = quantile(vector, probs = 0.975))
  
  return(data)
  
}
