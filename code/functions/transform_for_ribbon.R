transform_for_ribbon <- function(data, region, data_reefs, title = TRUE, ribbon_color, title_name){
  
  # 1. Transform the data ----
  
  cm <- data %>%
    filter(GCRMN_region == region) %>% 
    mutate(Year = as.numeric(Year)) %>%
    filter(Year < 2020) %>%
    mutate(rleid = with(rle(Data), rep(seq_along(lengths), lengths)),
           group = as.integer(rleid))
  
  cm1 <- cm %>% 
    ungroup %>% 
    mutate(d = 3) %>%
    uncount(d, .id = "A") %>%
    mutate_at(vars(Year, value, .lower_0.8, .upper_0.8, .lower_0.95, .upper_0.95),
              function(x=.) ifelse(.$A == 1,(x + lag(x))/2,
                                   ifelse(.$A == 3, (x + lead(x))/2, x))) %>%
    group_by_at(group_vars(cm)) %>%
    filter(row_number()!= 1, row_number() !=n()) %>% 
    ungroup() %>% 
    select(-A, -rleid)
  
  # 2. Make the plot ----
  
  if(title == TRUE){
    
    color_i <- data_reefs %>% 
      filter(GCRMN_region == region) %>% 
      select(color) %>% 
      pull()
    
    title_i <- data_reefs %>% 
      filter(GCRMN_region == region) %>% 
      mutate(GCRMN_region_portrait = paste0("<span style = 'color:", color,";'>", GCRMN_region, "</span> (", coral_reefs, "% of coral reefs)")) %>% 
      select(GCRMN_region_portrait) %>% 
      pull()
    
    plot_i <- ggplot(data = cm1) +
      geom_ribbon(aes(x = Year, ymin = .lower_0.95, ymax = .upper_0.95, fill = as.factor(Data), group = group), 
                  alpha = 0.25, show.legend = FALSE) +
      geom_ribbon(aes(x = Year, ymin = .lower_0.8, ymax = .upper_0.8, fill = as.factor(Data), group = group), 
                  alpha = 0.5, show.legend = FALSE) +
      geom_line(aes(x = Year, y = value, color = as.factor(Data), group = group), 
                linewidth = 1, show.legend = FALSE) +
      theme_graph() +
      theme(strip.background = element_blank(),
            strip.text = element_text(hjust = 0, size = 10),
            plot.title = element_markdown(size = rel(1)),
            plot.margin = margin(t = 5.5, r = 7, b = 5.5, l = 5.5, unit = "pt")) +
      labs(x = "Year", y = "Hard coral cover (%)", title = title_i) +
      scale_fill_manual(breaks = c("0", "1"), values = c("grey", color_i)) +
      scale_color_manual(breaks = c("0", "1"), values = c("grey", color_i))
    
  }else if(title == FALSE){
    
    color_i <- ribbon_color
    
    plot_i <- ggplot(data = cm1) +
      geom_ribbon(aes(x = Year, ymin = .lower_0.95, ymax = .upper_0.95, fill = as.factor(Data), group = group), 
                  alpha = 0.25, show.legend = FALSE) +
      geom_ribbon(aes(x = Year, ymin = .lower_0.8, ymax = .upper_0.8, fill = as.factor(Data), group = group), 
                  alpha = 0.5, show.legend = FALSE) +
      geom_line(aes(x = Year, y = value, color = as.factor(Data), group = group), 
                linewidth = 1, show.legend = FALSE) +
      theme_graph() +
      theme(plot.title = element_markdown(size = rel(1))) +
      labs(x = "Year", y = "Hard coral cover (%)", title = title_name) +
      scale_fill_manual(breaks = c("0", "1"), values = c("grey", color_i)) +
      scale_color_manual(breaks = c("0", "1"), values = c("grey", color_i))
    
  }
  
  return(plot_i)
  
}