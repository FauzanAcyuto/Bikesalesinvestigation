library(tidyverse)
library(janitor)
library(here)
library(tidyr)
install.packages("ggpubr")
library("ggpubr")

#relationship between revenue size and profit gap
Sales %>% 
  clean_names() %>% 
  mutate(
    real_profit = (order_quantity * unit_price) - (order_quantity * unit_cost),
    profit_gap = real_profit - profit,
    p_gap_percent = profit_gap/revenue*100
  ) %>% 
  ggplot() +
  geom_point(aes(x = revenue, y = profit_gap))+
  geom_smooth(aes(x = revenue, y = profit_gap))
  
#chart showing the relationship between date and profit gap
Sales %>% 
  clean_names() %>% 
  mutate(
    real_profit = (order_quantity * unit_price) - (order_quantity * unit_cost),
    profit_gap = real_profit - profit,
    p_gap_percent = profit_gap/revenue*100
  ) %>%
  ggplot() +
  geom_point(aes(x = date, y = profit_gap))+
  geom_smooth(aes(x = date, y = profit_gap))



#relationship between product sub category and average profit gap(suspected discount)
Sales %>% 
  clean_names() %>% 
  mutate(
    real_profit = (order_quantity * unit_price) - (order_quantity * unit_cost),
    profit_gap = real_profit - profit,
    p_gap_percent = profit_gap/revenue*100
  ) %>% 
  group_by(sub_category) %>% 
  summarize(avg_profit_gap_percent = mean(p_gap_percent),max_profit_gap_percent = max(p_gap_percent),
            min_profit_gap_percent = min(p_gap_percent))

#


#RESULTS:
#this exploration indicates a positive correlation between revenue and profit gap, although there
#are a lot oh exceptions, but statistically there is positive correlation
#there is no correlation between date and profit gap, this means that the discount is not time based
#there is no correlation between product sub category and profit gap(suspected discount)






