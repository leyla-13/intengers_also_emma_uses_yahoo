library(tidyverse)

data_all <- read_csv("politicians_data_all_rewritten.csv")
# print(data_all)

data <- filter(data_all,Party == "Republican Party (United States)") |>
  filter(n_Spouses>0)|>
  group_by(Party, n_Spouses)|>
  summarise(n=n())|>
  mutate(percentage=(n/sum(n))*100)|>
  print()

data <- filter(data_all,Party == "Democratic Party (United States)") |>
  filter(n_Spouses>0)|>
  group_by(Party, n_Spouses)|>
  summarise(n=n())|>
  mutate(percentage=(n/sum(n))*100)|>
  print()