list_chars = char_ranking_summary_2022$Name

transpose_df = char_ranking_summary_2022 %>%
  select(-Borda_2022, -GroupRank_2022) %>%
  gather("Participant", "Value", -Name) %>%
  spread(Name, Value)

names_list_1 = character(length(list_chars) ^ 2)
names_list_2 = character(length(list_chars) ^ 2)
mean_diff_list = numeric(length(list_chars) ^ 2)
median_diff_list = numeric(length(list_chars) ^ 2)
std_diff_list = numeric(length(list_chars) ^ 2)
within_1_list = numeric(length(list_chars) ^ 2)
within_3_list = numeric(length(list_chars) ^ 2)

index = 0

for (curr_char_1 in list_chars) {
  for (curr_char_2 in list_chars) {
    
    index = index + 1
    
    if (curr_char_1 == curr_char_2) {
      curr_mean_diff = NA
      curr_median_diff = NA
      curr_std_diff = NA
      curr_within_1 = NA
      curr_within_3 = NA
    } else {
      curr_df = transpose_df %>%
        select(curr_char_1, curr_char_2) %>%
        mutate(
          Diff = abs(!!sym(curr_char_2) - !!sym(curr_char_1)),
          Within1 = (Diff <= 1),
          Within3 = (Diff <= 3)
          ) %>%
        summarise(
          Mean_Diff = mean(Diff),
          Median_Diff = median(Diff),
          Std_Diff = sd(Diff),
          Within1_Sum = sum(Within1),
          Within3_Sum = sum(Within3)
        )
      curr_mean_diff = curr_df$Mean_Diff[1]
      curr_median_diff = curr_df$Median_Diff[1]
      curr_std_diff = curr_df$Std_Diff[1]
      curr_within_1 = curr_df$Within1_Sum[1]
      curr_within_3 = curr_df$Within3_Sum[1]
    }
    
    names_list_1[index] = curr_char_1
    names_list_2[index] = curr_char_2
    mean_diff_list[index] = curr_mean_diff
    median_diff_list[index] = curr_median_diff
    std_diff_list[index] = curr_std_diff
    within_1_list[index] = curr_within_1
    within_3_list[index] = curr_within_3
  }
}

adjacency_df = data.frame(
  names_list_1, names_list_2,
  mean_diff_list, median_diff_list, std_diff_list,
  within_1_list, within_3_list
)

adjacency_df %>%
  ggplot(aes(x = names_list_1, y = names_list_2, fill = mean_diff_list)) +
  geom_raster() +
  theme_bw() +
  # Because we need the x and y axis to display every node,
  # not just the nodes that have connections to each other,
  # make sure that ggplot does not drop unused factor levels
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  theme(
    # Rotate the x-axis lables so they are legible
    axis.text.x = element_text(angle = 270, hjust = 0),
    # Force the plot into a square aspect ratio
    aspect.ratio = 1) +
  labs(x = "", y = "", fill = "Mean Diff")
