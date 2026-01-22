 library(tidyverse)

#rewrite to get proportion of people with more than one spouse
  

data <- read_csv("new_cleaner_beautiful_data.csv") |>
  mutate(generation=fct_relevel(generation, c("greatest_generation", "silent_generation", "baby_boomers", "generation_x", "generation_y")))|>
  mutate(dummy_spouse = case_when(
    n_spouses == 1 ~ 0,
    n_spouses > 1 ~ 1,
    TRUE ~ NA
  )) |> 
  mutate(dummy_child = case_when(
    n_children == 0 ~ 0,
    n_children > 0 ~ 1,
    TRUE ~ NA
  ))|>
  filter(generation=="greatest_generation"|generation=="silent_generation"|generation=="baby_boomers"|generation=="generation_x")



ggplot(data, aes(x=generation, y=dummy_spouse, fill=party)) +
  geom_bar(stat="summary", fun=mean, position="dodge")+
  scale_fill_manual(values=c("Democratic Party (United States)"="#1E1BE3", "Republican Party (United States)"="#E31B1B"), labels=c("Democratic", "Republican"))+
  labs(x="Generation",
      y="Percentage of politicians \n that remarried",
      fill="Political party")+
  scale_x_discrete(labels=c("Greatest Generation","Silent Generation", "Baby Boomers", "Generation X", "Generation Y"))+
  scale_y_continuous(labels=scales::label_percent())
  
ggsave("Percentage of politicians that remarried vs Generation per political party.pdf", width=30, height=10, units="cm")