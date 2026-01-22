library(tidyverse)
#this uses the less cleaned version, it does include the data for campaigns and presidencies
data_all <- read_csv("politicians_data_all_rewritten.csv")

data_great_reversal <- filter(data_all, Birthyear >= 1901)

#percentage no spouses of total republicans
data <- filter(data_great_reversal,Party == "Republican Party (United States)") |>
  group_by(Party, n_Spouses)|>
  summarise(n=n())|>
  mutate(percentage=(n/sum(n))*100)|>
  print()

#percentage more than 1 spouse of all republicans with spouses
data <- filter(data_great_reversal,Party == "Republican Party (United States)") |>
  filter(n_Spouses>0)|>
  group_by(Party, n_Spouses)|>
  summarise(n=n())|>
  mutate(percentage=(n/sum(n))*100)|>
  print()

#percentage no spouses of total democrats
data <- filter(data_great_reversal,Party == "Democratic Party (United States)") |>
  group_by(Party, n_Spouses)|>
  summarise(n=n())|>
  mutate(percentage=(n/sum(n))*100)|>
  print()

#percentage more than 1 spouse of all democrats with spouses
data <- filter(data_great_reversal,Party == "Democratic Party (United States)") |>
  filter(n_Spouses>0)|>
  group_by(Party, n_Spouses)|>
  summarise(n=n())|>
  mutate(percentage=(n/sum(n))*100)|>
  print()

#percentage of republicans that have spouse AND child
data <- filter(data_great_reversal, Party == "Republican Party (United States)", n_Spouses>0)|>
  group_by(Party, n_Spouses, n_Children)|>
  summarise(n=n())|>
  mutate(percentage=(n/sum(n))*100)|>
  print()

#percentage of democrats that have spouse AND child
data <- filter(data_great_reversal, Party == "Democratic Party (United States)", n_Spouses>0)|>
  group_by(Party, n_Spouses, n_Children)|>
  summarise(n=n())|>
  mutate(percentage=(n/sum(n))*100)|>
  print()