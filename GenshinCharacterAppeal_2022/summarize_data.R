CalculateBorda = function(array_to_calc, num_candidates = 48) {
  borda_count = (num_candidates * length(array_to_calc)) - sum(array_to_calc)
  return(borda_count)
}

char_tier_summary_2021 = data_tier_2021 %>%
  mutate(
    Mean_2021 = select(., David:John) %>% apply(1, mean),
    Median_2021 = select(., David:John) %>% apply(1, median),
    Std_2021 = select(., David:John) %>% apply(1, sd),
  )

char_tier_summary_2022 = data_tier_2022 %>%
  mutate(
    Mean_2022 = select(., Frozen:Qihan) %>% apply(1, mean),
    Median_2022 = select(., Frozen:Qihan) %>% apply(1, median),
    Std_2022 = select(., Frozen:Qihan) %>% apply(1, sd),
    )

char_ranking_summary_2021 = data_ranking_2021 %>%
  mutate(
    Borda_2021 = select(., David:John) %>% apply(1, CalculateBorda)
  ) %>%
  arrange(desc(Borda_2021)) %>%
  mutate(GroupRank_2021 = row_number()) %>%
  arrange(Name)

char_ranking_summary_2022 = data_ranking_2022 %>%
  mutate(
    Borda_2022 = select(., Frozen:Qihan) %>% apply(1, CalculateBorda)
  ) %>%
  arrange(desc(Borda_2022)) %>%
  mutate(GroupRank_2022 = row_number()) %>%
  arrange(Name)
  