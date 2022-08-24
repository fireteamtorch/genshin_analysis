NumberToTierGrade = function(number) {
  if (number > 4.8333) {
    tier = "S"
  } else if (number > 4.5) {
    tier = "S-"
  } else if (number > 4.1666) {
    tier = "A+"
  } else if (number > 3.8333) {
    tier = "A"
  } else if (number > 3.5) {
    tier = "A-"
  } else if (number > 3.1666) {
    tier = "B+"
  } else if (number > 2.8333) {
    tier = "B"
  } else if (number > 2.5) {
    tier = "B-"
  } else if (number > 2.1666) {
    tier = "C+"
  } else if (number > 1.8333) {
    tier = "C"
  } else if (number > 1.5) {
    tier = "C-"
  } else if (number > 1.1666) {
    tier = "D+"
  } else {
    tier = "D"
  }
  return(tier)
}

character_tiers_2021_merged = char_tier_summary_2021 %>%
  select(Name, Mean_2021, Median_2021, Std_2021) %>%
  merge(
    char_ranking_summary_2021 %>%
      select(Name, Borda_2021, GroupRank_2021)
  ) %>%
  mutate(
    Percentile_2021 = 100*(n() - GroupRank_2021)/n()
  )

character_tiers_2022_merged = char_tier_summary_2022 %>%
  select(Name, Mean_2022, Median_2022, Std_2022) %>%
  merge(
    char_ranking_summary_2022 %>%
      select(Name, Borda_2022, GroupRank_2022)
  ) %>%
  mutate(
    Percentile_2022 = 100*(n() - GroupRank_2022)/n()
  )

character_tiers_both_years = character_tiers_2021_merged %>%
  merge(character_tiers_2022_merged) %>%
  mutate(
    Mean_Diff = Mean_2022 - Mean_2021,
    Median_Diff = Median_2022 - Median_2021,
    Std_Diff = Std_2022 - Std_2021,
    Borda_Diff = Borda_2022 - Borda_2021,
    GroupRank_Diff = GroupRank_2022 - GroupRank_2021,
    Percentile_Diff = Percentile_2022 - Percentile_2021,
  )
character_tier_gathered = character_tiers_both_years %>%
  gather("Key", "Value", -Name) %>%
  separate(Key, into = c("Metric", "Year"), sep = "_") %>%
  spread(Metric, Value) %>%
  rowwise() %>%
  mutate(Tier = NumberToTierGrade(Mean))

plot_by_year = character_tier_gathered %>%
  filter(Year != "Diff") %>%
  group_by(Name) %>%
  mutate(SortBy = first(Percentile)) %>%
  ungroup() %>%
  ggplot(
    aes(x = reorder(Name, -SortBy), y = Percentile, fill = Year)
  ) +
  geom_bar(stat="identity", color="black", size = 0.1, position = "dodge") +
  theme_classic() +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  xlab("Character") +
  ylab("Percentile")
show(plot_by_year)

plot_by_percentile_change = character_tiers_both_years %>%
  ggplot(
    aes(x = reorder(Name, -Percentile_Diff), y = Percentile_Diff)
  ) +
  geom_bar(stat="identity", color="black", fill = "lightblue", size = 0.1) +
  theme_classic() +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  xlab("Character") +
  ylab("Percentile Change")
show(plot_by_percentile_change)

character_ranks_byperson_2021 = char_ranking_summary_2021 %>%
  select(-Borda_2021, -GroupRank_2021) %>%
  gather("Participant", "Rank_2021", -Name) %>%
  group_by(Participant) %>%
  mutate(Percentile_2021 = 100*(n() - Rank_2021)/n()) %>%
  ungroup()

character_ranks_byperson_2022 = char_ranking_summary_2022 %>%
  select(-Borda_2022, -GroupRank_2022) %>%
  gather("Participant", "Rank_2022", -Name) %>%
  group_by(Participant) %>%
  mutate(Percentile_2022 = 100*(n() - Rank_2022)/n()) %>%
  ungroup()

character_ranks_both_years = character_ranks_byperson_2021 %>%
  merge(character_ranks_byperson_2022) %>%
  mutate(
    Rank_Diff = Rank_2022 - Rank_2021,
    Percentile_Diff = Percentile_2022 - Percentile_2021
  ) %>%
  select(Participant, Name, Percentile_2021, Percentile_2022, Percentile_Diff) %>%
  arrange(Participant, Percentile_Diff)
