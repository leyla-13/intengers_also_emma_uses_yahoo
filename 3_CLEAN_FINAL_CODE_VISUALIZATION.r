
## PART 2 - VISUALIZATION
## please install these packages if they are not already
library(stats)
library(tidyverse)
library(ggplot2)
library(jtools)
library(dplyr)

#categorizing no remarriage vs remarriage for relative calculations
new_dat <- read_csv("new_cleaner_beautiful_data.csv") |>
  mutate(dummy_spouse = case_when(
    n_spouses == 1 ~ 0,
    n_spouses > 1 ~ 1,
    TRUE ~ NA
  )) |> 
  mutate(dummy_child = case_when(
    n_children == 0 ~ 0,
    n_children > 0 ~ 1,
    TRUE ~ NA
  ))

#showing number of politicians that have one spouse and more than one spouse
spouse_number_analysis <- group_by(new_dat, party, dummy_spouse)|>
  summarise(n=n())|>
  print()

#showing number of politicians per party that have children and no children
children_number_analysis <- group_by(new_dat, party, dummy_child)|>
  summarise(n=n())|>
  print()

#showing number of polticians per party per generation
generation_number_analysis <- group_by(new_dat, party, generation)|>
  summarise(n=n())|>
  print()

#showing number of politicians per party per religious subgroup
religion_number_analysis <- group_by(new_dat, party, religion)|>
  summarise(n=n())|>
  print()

# calculating SD and varience to check data comparability
sum_rep <- new_dat |>
  group_by(party) |>
  summarize(var_spouse = var(n_spouses),
  var_child = var(n_children),
  sd_spouse = sd(n_spouses),
  sd_child = sd(n_children))

print(sum_rep)



# making the variables factors for ordered data
order_dat <- new_dat |>
  filter(!is.na(religion)) |> 
  mutate(religion = fct_relevel(religion, c("Conservative Christian", "Progressive Christian", "Undefined Christian", "Non Christian")) 
)

# visualizing relative number of remarriages for republicans vs democrats

relative_remarriage <- new_dat |>
  group_by(party)|>
  summarise(relative_remarriage = mean(dummy_spouse, na.rm = TRUE)) |>
  mutate(percentage = 100 * relative_remarriage)

remarriage_party_bar <-  relative_remarriage |> 
  ggplot(mapping = aes(x = party, y = relative_remarriage, fill = party)) +
  geom_col(position = "dodge") +
  labs(x = "Political Party",
  y = "Percentage of Remarried Politicians") +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_discrete(labels=c("Democratic", "Republican")) +
  theme_apa(legend.pos = "right",
  legend.use.title = FALSE,
  legend.font.size = 12,
  x.font.size = 12,
  y.font.size = 12,
  facet.title.size = 12,
  remove.y.gridlines = TRUE,
  remove.x.gridlines = TRUE) +
  theme(legend.position="none") +
  scale_fill_manual(values=c("#1E1BE3","#E31B1B"))
  
print(remarriage_party_bar)

ggsave('bar_plot_spouse.pdf', plot = remarriage_party_bar, width = 10 , height = 10, units = "cm")


# visualizing relative remarriage grouped by party per religion

religion_remarriage <- order_dat |>
  filter(religion %in% c( "Conservative Christian","Progressive Christian","Undefined Christian"))   |>
ggplot(mapping = aes(x=religion, y=dummy_spouse, fill=party)) +
  geom_col(stat="summary", fun=mean, position="dodge")+
  scale_fill_manual(values=c("Democratic Party (United States)"="#1E1BE3", "Republican Party (United States)"="#E31B1B"), labels=c("Democratic", "Republican"))+
  labs(x="Religion Category",
      y="Percentage of Remarried Politicians",
      fill="Political party")+
  scale_x_discrete(labels=c("Christian \n (Conservative)", "Christian \n (Progressive)", "Christian  \n (Undefined)", "Non-Christian"))+
  scale_y_continuous(labels=scales::label_percent()) +   
  theme_apa(  legend.pos = "right",
  legend.use.title = FALSE,
  legend.font.size = 12,
  x.font.size = 12,
  y.font.size = 12,
  facet.title.size = 12,
  remove.y.gridlines = TRUE,
  remove.x.gridlines = TRUE)

print(religion_remarriage)
ggsave("Percentage of politicians that remarried vs Religion per political party.pdf", width=30, height=15, units="cm")


# visalizing relative remarriage grouped by party per generation
generation_remarriage <- new_dat |>
  mutate(generation=fct_relevel(generation, c("greatest_generation", "silent_generation", "baby_boomers", "generation_x", "generation_y")))|>
  filter(generation %in% c( "greatest_generation","silent_generation","baby_boomers","generation_x")) |>
ggplot(mapping = aes(x=generation, y=dummy_spouse, fill=party)) +
  geom_col(stat="summary", fun=mean, position="dodge")+
  scale_fill_manual(values=c("Democratic Party (United States)"="#1E1BE3", "Republican Party (United States)"="#E31B1B"), labels=c("Democratic", "Republican"))+
  labs(x= "Generation",
      y="Percentage of Remarried Politicians",
      fill="Political Party")+
  scale_x_discrete(labels=c("Greatest Generation","Silent Generation", "Baby Boomers", "Generation X", "Generation Y"))+
  scale_y_continuous(labels=scales::label_percent()) +   
  theme_apa(  legend.pos = "right",
  legend.use.title = FALSE,
  legend.font.size = 12,
  x.font.size = 12,
  y.font.size = 12,
  facet.title.size = 12,
  remove.y.gridlines = TRUE,
  remove.x.gridlines = TRUE)

print(generation_remarriage)
 
ggsave("Percentage of politicians that remarried vs Generation per political party.pdf", plot = generation_remarriage, width=30, height=15, units="cm")
