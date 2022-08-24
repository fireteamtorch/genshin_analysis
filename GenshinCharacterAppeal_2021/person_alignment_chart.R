library("tidyverse")

raw_tier_data = read.csv("Data/genshin_character_survey_tier_data.csv")

gathered_tier_data = raw_tier_data %>%
  rename(Character = Name) %>%
  gather(key = "Person", value = "Score", -Character)

mean_rating_by_person = gathered_tier_data %>%
  group_by(Person) %>%
  summarise(Mean = mean(Score))

mean_rating_by_character = gathered_tier_data %>%
  group_by(Character) %>%
  summarise(Mean = mean(Score))

mean_deviation_by_person = gathered_tier_data %>%
  merge(mean_rating_by_character, by = "Character") %>%
  mutate(Deviation = abs(Score - Mean)) %>%
  group_by(Person) %>%
  summarise(MeanDev = mean(Deviation))

combined_person_data = mean_rating_by_person %>%
  rename(MeanScore = Mean) %>%
  merge(mean_deviation_by_person, by = "Person") %>%
  mutate(
    MeanDevAdj = MeanDev - median(MeanDev),
    MeanScoreAdj = MeanScore - median(MeanScore)
  )

combined_person_plot = combined_person_data %>%
  ggplot(aes(x = MeanDevAdj, y = MeanScoreAdj)) +
  geom_vline(xintercept = 0, color = "black", size = 1) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
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
    ) #+
  #xlim(-0.35, 0.35) +
  #ylim(-0.8, 0.8)
  #annotate("text", x = 0, y = 0.8, size = 4, label = "  Soft", hjust = 0) +
  #annotate("text", x = 0, y = -0.8, size = 4, label = "  Harsh", hjust = 0) +
  #annotate("text", x = -0.35, y = 0, size = 4, label = "Basic", hjust = 0, vjust = -0.5) +
  #annotate("text", x = 0.35, y = 0, size = 4, label = "Deviant", hjust = 1, vjust = -0.5) +
  #ggtitle("Character Preference Political Compass")

show(combined_person_plot)
