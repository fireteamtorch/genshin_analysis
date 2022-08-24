char_attributes_merged = char_tier_summary_2022 %>%
  select(Name, Mean_2022, Median_2022, Std_2022) %>%
  merge(
    char_ranking_summary_2022 %>%
      select(Name, Borda_2022, GroupRank_2022),
    by = "Name"
  ) %>%
  merge(
    character_metadata,
    by = "Name"
  )

tier_scores_by_bodytype = char_attributes_merged %>%
  group_by(`Body Type`) %>%
  summarise(
    `Mean Tier` = mean(Mean_2022) - 1,
    `Std Tier` = sd(Mean_2022)
    )
plot_by_bodytype = tier_scores_by_bodytype %>%
  ggplot(
    aes(x = reorder(`Body Type`, -`Mean Tier`), y = `Mean Tier`)
  ) +
  geom_bar(stat="identity", color="black", fill="lightblue", size = 0.1) +
  geom_errorbar(aes(ymin=`Mean Tier`-`Std Tier`, ymax=`Mean Tier`+`Std Tier`), width=.2, size = 0.2) + 
  theme_classic() +
  xlab("Body Type") +
  ylab("Mean Tier") +
  scale_y_continuous(breaks = c(0,1,2,3,4), labels = c("D","C","B","A","S"))
show(plot_by_bodytype)

tier_scores_by_region = char_attributes_merged %>%
  filter(`Affiliated Area` != "Outlander") %>%
  group_by(`Affiliated Area`) %>%
  summarise(
    `Mean Tier` = mean(Mean_2022) - 1,
    `Std Tier` = sd(Mean_2022)
  )
plot_by_region = tier_scores_by_region %>%
  ggplot(
    aes(x = reorder(`Affiliated Area`, -`Mean Tier`), y = `Mean Tier`, fill = reorder(`Affiliated Area`, -`Mean Tier`))
  ) +
  geom_bar(stat="identity", color="black", size = 0.1) +
  geom_errorbar(aes(ymin=`Mean Tier`-`Std Tier`, ymax=`Mean Tier`+`Std Tier`), width=.2, size = 0.2) + 
  theme_classic() +
  xlab("Affiliated Region") +
  ylab("Mean Tier") +
  scale_y_continuous(breaks = c(0,1,2,3,4), labels = c("D","C","B","A","S")) + 
  scale_fill_manual("legend", values = c("Mondstadt" = "#74C2A8", "Liyue" = "#FAB632", "Inazuma" = "#AF8EC1")) +
  guides(fill="none")
show(plot_by_region)

tier_scores_by_element = char_attributes_merged %>%
  filter(Vision != "Unknown") %>%
  group_by(Vision) %>%
  summarise(
    `Mean Tier` = mean(Mean_2022) - 1,
    `Std Tier` = sd(Mean_2022)
  )
plot_by_element = tier_scores_by_element %>%
  ggplot(
    aes(x = reorder(Vision, -`Mean Tier`), y = `Mean Tier`, fill = Vision)
  ) +
  geom_bar(stat="identity", color="black", size = 0.1) +
  geom_errorbar(aes(ymin=`Mean Tier`-`Std Tier`, ymax=`Mean Tier`+`Std Tier`), width=.2, size = 0.2) + 
  theme_classic() +
  xlab("Vision Element") +
  ylab("Mean Tier") +
  scale_y_continuous(breaks = c(0,1,2,3,4), labels = c("D","C","B","A","S")) + 
  scale_fill_manual("legend", values = c("Anemo" = "#74C2A8", "Geo" = "#FAB632", "Electro" = "#AF8EC1", "Pyro" = "#EF7938", "Hydro" = "#4CC2F1", "Cryo" = "#9FD6E3", "Dendro" = "#A5C83B")) +
  guides(fill="none")
show(plot_by_element)

tier_scores_by_rarity = char_attributes_merged %>%
  filter(`Rarity 2` != "Outlander") %>%
  group_by(`Rarity 2`) %>%
  summarise(
    `Mean Tier` = mean(Mean_2022) - 1,
    `Std Tier` = sd(Mean_2022)
  )
plot_by_rarity = tier_scores_by_rarity %>%
  ggplot(
    aes(x = reorder(`Rarity 2`, -`Mean Tier`), y = `Mean Tier`, fill = `Rarity 2`)
  ) +
  geom_bar(stat="identity", color="black", size = 0.1) +
  geom_errorbar(aes(ymin=`Mean Tier`-`Std Tier`, ymax=`Mean Tier`+`Std Tier`), width=.2, size = 0.2) + 
  theme_classic() +
  xlab("Rarity") +
  ylab("Mean Tier") +
  scale_y_continuous(breaks = c(0,1,2,3,4), labels = c("D","C","B","A","S")) + 
  scale_fill_manual("legend", values = c("Limited 5*" = "#FFCB3C", "Standard 5*" = "#BE762C", "4*" = "#8E6DB4")) +
  guides(fill="none")
show(plot_by_rarity)

tier_scores_by_updates = char_attributes_merged %>%
  select(Name, Mean_2022, Borda_2022, `First Appears`, `Last Appears`) %>%
  gather("Update" , "Value", -Name, -Mean_2022, -Borda_2022)
tier_scores_by_updates$Value[tier_scores_by_updates$Value == "0.0"] = "Never"
tier_scores_by_updates$Value = factor(tier_scores_by_updates$Value, levels = c(
  "Never", "1.0", "1.1", "1.2", "1.3", "1.4", "1.5", "1.6", "2.0", "2.1", "2.2", "2.3", "2.4"
  )
)
tier_scores_by_updates = tier_scores_by_updates %>%
  group_by(Update, Value) %>%
  summarise(
    `Mean Tier` = mean(Mean_2022) - 1,
    `Std Tier` = sd(Mean_2022),
    `Mean Borda` = mean(Borda_2022),
    `Std Borda` = sd(Borda_2022),
  )
plot_by_update_mean = tier_scores_by_updates %>%
  ggplot(
    aes(x = Value, y = `Mean Tier`, color = Update, group=Update)
  ) +
  geom_line(stat="identity", linetype = "solid", alpha = 0.8) +
  geom_point(stat="identity")+
  geom_errorbar(aes(ymin=`Mean Tier`-`Std Tier`, ymax=`Mean Tier`+`Std Tier`), width=.2, size = 0.2) + 
  theme_classic() +
  xlab("Update") +
  ylab("Mean Tier") +
  scale_y_continuous(breaks = c(0,1,2,3,4), labels = c("D","C","B","A","S")) + 
  guides(fill="none")
show(plot_by_update_mean)
