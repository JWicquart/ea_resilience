before_after_interval_1 <- function(year_before, year_after, region, year_bin){
  
  # 1. Generate a normal distribution
  
  # 1.1 For year_before
  
  mean_before <- data_overall %>% filter(Year == year_before & GCRMN_region == region) %>% select(value) %>% pull()
  
  lower_80 <- data_overall %>% filter(Year == year_before & GCRMN_region == region) %>% select(.lower_0.8) %>% pull()
  
  upper_80 <- data_overall %>% filter(Year == year_before & GCRMN_region == region) %>% select(.upper_0.8) %>% pull()
  
  sd_before <- mean(mean_before-lower_80, upper_80-mean_before)
  
  dist_before <- rnorm(1000, mean = mean_before, sd = sd_before)
  
  # 1.2 For year_after
  
  mean_after <- data_overall %>% filter(Year == year_after & GCRMN_region == region) %>% select(value) %>% pull()
  
  lower_80 <- data_overall %>% filter(Year == year_after & GCRMN_region == region) %>% select(.lower_0.8) %>% pull()
  
  upper_80 <- data_overall %>% filter(Year == year_after & GCRMN_region == region) %>% select(.upper_0.8) %>% pull()
  
  sd_after <- mean(mean_after-lower_80, upper_80-mean_after)
  
  dist_after <- rnorm(1000, mean = mean_after, sd = sd_after)
  
  # 2. Get difference based on random sampling
  
  vector <- numeric(0)
  
  for (i in 1:1000){
    
    vector[i] <- (sample(dist_after, size = 1) - sample(dist_before, size = 1))
    
  }
  
  # 3. Calculate CI
  
  error_095 <- qnorm(0.975)*sd(vector)/sqrt(length(vector))
  
  data <- tibble(year_bin = year_bin,
                 year_before = year_before,
                 year_after = year_after,
                 region = region,
                 mean = mean(vector),
                 ic95_lower = quantile(vector, probs = 0.025),
                 ic95_upper = quantile(vector, probs = 0.975))
  
  return(data)
  
}
