plot_tier_hist = char_tier_summary_2022 %>%
  ggplot(aes(x=Mean_2022)) + 
  geom_histogram(binwidth=0.1, color="black", fill="lightblue", size = 0.1) + 
  theme_classic() +
  xlab("Mean Tier Ranking") +
  ylab("Count") +
  scale_x_continuous(breaks = c(1,2,3,4,5), labels = c("D","C","B","A","S"), limits = c(1,5))
show(plot_tier_hist)  

plot_tier_hist_median = char_tier_summary_2022 %>%
  select(-Mean_2022, -Median_2022, -Std_2022) %>%
  gather(key = "key", value = "value", -Name) %>%
  ggplot(aes(x=value)) + 
  geom_histogram(binwidth=0.1, color="black", fill="lightblue", size = 0.1) + 
  theme_classic() +
  xlab("Tier Ranking") +
  ylab("Count") +
  scale_x_continuous(breaks = c(1,2,3,4,5), labels = c("D","C","B","A","S"))
show(plot_tier_hist_median)  
