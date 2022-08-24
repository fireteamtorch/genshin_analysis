data_ranking_2022 = read_excel("data/gcas_v2.xlsx", sheet = 1)
data_tier_2022 = read_excel("data/gcas_v2.xlsx", sheet = 2)
character_metadata = read_excel("data/gcas_v2.xlsx", sheet = 3)

data_ranking_2021 = read_excel("data/gcas_v1.xlsx", sheet = 1) %>%
  rename(Frozen = Yuhan)
data_tier_2021 = read_excel("data/gcas_v1.xlsx", sheet = 2) %>%
  rename(Frozen = Yuhan)
