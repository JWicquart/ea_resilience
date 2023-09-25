before_after_interval_2 <- function(year, region){
  
  # 1. Generate a normal distribution
  
  # 1.1 For year_before
  
  data <- data_overall %>% 
    filter(Year %in% c(year-3, year-2, year-1) & GCRMN_region == region) %>% 
    summarise_at(c("value", ".lower_0.8", ".upper_0.8"), mean)
  
  sd <- mean(data$value-data$.lower_0.8, data$.upper_0.8-data$value)
  
  dist_before <- rnorm(1000, mean = data$value, sd = sd)
  
  # 1.2 For year
  
  data <- data_overall %>% 
    filter(Year %in% c(year+3, year+2, year+1) & GCRMN_region == region) %>% 
    summarise_at(c("value", ".lower_0.8", ".upper_0.8"), mean)
  
  sd <- mean(data$value-data$.lower_0.8, data$.upper_0.8-data$value)
  
  dist_after <- rnorm(1000, mean = data$value, sd = sd)
  
  # 2. Get difference based on random sampling
  
  vector <- numeric(0)
  
  for (i in 1:1000){
    
    vector[i] <- (sample(dist_after, size = 1) - sample(dist_before, size = 1))
    
  }
  
  # 3. Calculate CI
  
  data <- tibble(year = year,
                 region = region,
                 mean = mean(vector),
                 ic95_lower = quantile(vector, probs = 0.025),
                 ic95_upper = quantile(vector, probs = 0.975))
  
  return(data)
  
}
