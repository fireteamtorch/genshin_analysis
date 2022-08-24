char_ranking_summary_2022 %>%
  ggplot(aes(x=Borda_2022)) + 
  geom_histogram(binwidth=20, color="black", fill="lightblue", size = 0.1) + 
  theme_classic() +
  xlab("Borda Count") +
  ylab("Count")

per_person_borda_deviation = char_ranking_summary_2022 %>%
  mutate(
    across(
      # Select cols
      colnames(char_ranking_summary_2022)[2:17],
      # lambda style function -- can use others
      ~.x - GroupRank,
      # Takes the column name and adds '-1'
      .names = "{.col}_Borda_Dev"
    ),
  ) %>%
  select(-(2:18))
