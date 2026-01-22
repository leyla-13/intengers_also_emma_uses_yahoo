install.packages("jtools")
library(tidyverse)
library(ggplot2)
library(jtools)


  
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




#looking at the zero distribution for children


# making the variables factors
order_dat <- new_dat |>
  filter(!is.na(religion)) |> 
  mutate(religion = fct_relevel(religion, c("Conservative Christian", "Progressive Christian", "Undefined Christian", "Non Christian")) 
)

# distribution in data
distribution_child <-  new_dat |> 
  ggplot(mapping = aes(x = n_children, fill = party)) +
  geom_bar(position = "dodge") +
  labs(x = "children count") +
  scale_fill_manual(values=c("#1E1BE3","#E31B1B")) +
  theme_apa(  legend.pos = "right",
  legend.use.title = FALSE,
  legend.font.size = 12,
  x.font.size = 12,
  y.font.size = 12,
  facet.title.size = 12,
  remove.y.gridlines = TRUE,
  remove.x.gridlines = TRUE) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

#looking at the zero distribution for remarriage
distribution_spouse <-  new_dat |> 
  ggplot(mapping = aes(x = n_spouses, fill = party)) +
  geom_bar(position = "dodge") +
  labs(x = "Number of Marriages", y = "Number of Politicians", fill=" Political Party") +
  scale_fill_manual(values=c("#1E1BE3","#E31B1B"), labels = c("Democratic", "Republican")) +
  theme_apa( legend.pos = "right",
  legend.use.title = TRUE,
  legend.font.size = 12,
  x.font.size = 12,
  y.font.size = 12,
  facet.title.size = 12,
  remove.y.gridlines = TRUE,
  remove.x.gridlines = TRUE) +
  scale_x_continuous(breaks = seq(1,6, by = 1))


#religion distribution per party
distribution_religion <-  order_dat |> 
  ggplot(mapping = aes(x = religion, fill = party)) +
  geom_bar(position = "dodge") +
  labs(x = "Religion Category") +
  scale_fill_manual(values=c("#1E1BE3","#E31B1B")) +
  scale_x_discrete(labels=c("Christian (Conservative)", "Christian (Progressive)", "Christian (Undefined)", "Non-Christian"))+
  theme_apa(  legend.pos = "right",
  legend.use.title = FALSE,
  legend.font.size = 12,
  x.font.size = 12,
  y.font.size = 12,
  facet.title.size = 12,
  remove.y.gridlines = TRUE,
  remove.x.gridlines = TRUE) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))




print(distribution_child)
print(distribution_spouse)
print(distribution_religion)

ggsave('distribution_child.pdf', plot = distribution_child, width = 20 , height = 8, units = "cm")
ggsave('distribution_spouse.pdf', plot = distribution_spouse, width = 20 , height = 8, units = "cm")
ggsave('distribution_religion.pdf', plot = distribution_religion, width = 20 , height = 12, units = "cm")


#variation analysis

sum_rep <- new_dat |>
  group_by(party) |>
  summarize(var_spouse = var(n_spouses),
  var_child = var(n_children),
  sd_spouse = sd(n_spouses),
  sd_child = sd(n_children))

print(sum_rep)


## bar plot dem-rep difference
## add the numbers average on top of the bar and make the party names normal 
# (democrat), average number of children instead of number of children and 
# colors per column

#remarriage comparison between dems and reps
# summary_spouse_remarriage <- new_dat |>
#   group_by(party)|>
#   summarise(avg_n_remarriage = mean(remarriage, na.rm = TRUE))

# #average children per party
# summary_kids_non <- new_dat |>
#   group_by(party)|>
#   summarise(avg_n_children_non = mean(n_children, na.rm = TRUE))

#remarriage comparison between dems and reps
relative_remarriage <- new_dat |>
  group_by(religion, party)|>
  summarise(relative_remarriage = mean(dummy_spouse, na.rm = TRUE)) |>
  mutate(percentage = 100 * relative_remarriage)



#bar graphs for remarriage
dem_rep_bar <-  relative_remarriage |> 
  ggplot(mapping = aes(x = religion, y = relative_remarriage, fill = party)) +
  geom_col(position = "dodge") +
  labs(x = "Religion Category",
  y = "Percentage of Remarried Politicians") +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_manual(values=c("#1E1BE3","#E31B1B")) +
  theme_apa(  legend.pos = "right",
  legend.use.title = FALSE,
  legend.font.size = 12,
  x.font.size = 12,
  y.font.size = 12,
  facet.title.size = 12,
  remove.y.gridlines = TRUE,
  remove.x.gridlines = TRUE) +
  theme(axis.text.x = element_text(angle = 15, vjust = 0.5)) +
  geom_text(aes(label=round(percentage, 2)), vjust= 1, hjust = 1)

print(dem_rep_bar)

ggsave('bar_plot_spouse.pdf', plot = dem_rep_bar, width = 25 , height = 10, units = "cm")

#bar graph for kid number
dem_rep_bar_kid <-  summary_kids_non |> 
  ggplot(mapping = aes(x = party, y = avg_n_children_non)) +
  geom_col(position = "dodge", color = "#000000ff", fill = "#00000082") +
  labs(x = "Party",
  y = "Number of Children") +
    theme_apa(  legend.pos = "right",
  legend.use.title = FALSE,
  legend.font.size = 12,
  x.font.size = 12,
  y.font.size = 12,
  facet.title.size = 12,
  remove.y.gridlines = TRUE,
  remove.x.gridlines = TRUE) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

print(dem_rep_bar_kid)

ggsave('bar_plot_kid.pdf', plot = dem_rep_bar_kid, width = 8 , height = 10, units = "cm")


  