avg_rankings_by_person_2022 = char_tier_summary_2022 %>%
  select(Frozen:Qihan) %>%
  summarize_all(mean)

deviations_2022 = as.data.frame(char_tier_summary_2022)
deviations_2022[2:(ncol(deviations_2022)-3)] = abs(deviations_2022[2:(ncol(deviations_2022)-3)] - deviations_2022[,(ncol(deviations_2022)-2)])
avg_deviations_by_person_2022 = deviations_2022 %>%
  select(Frozen:Qihan) %>%
  summarize_all(mean)

person_data_agg = data.frame(
  Person = colnames(avg_rankings_by_person_2022),
  Rating = t(avg_rankings_by_person_2022[1,]),
  Deviation = t(avg_deviations_by_person_2022[1,])
)
rownames(person_data_agg) = NULL
colnames(person_data_agg) = c("Person", "Rating", "Deviation")


combined_person_plot = person_data_agg %>%
  ggplot(aes(x = Rating, y = Deviation)) +
  geom_vline(xintercept = 3.06, color = "black", size = 0.5) +
  geom_hline(yintercept = 0.98, color = "black", size = 0.5) +
  geom_text(aes(label = Person), size = 3.5, family = "Decima WE", color = "grey20", vjust = 0.5, hjust = 0.5) +
  xlab("") +
  ylab("") +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

show(combined_person_plot)

