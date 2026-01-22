library(tidyverse)
new_dat <- read_csv("new_cleaner_beautiful_data.csv")


## bar plot dem-rep difference
## add the numbers average on top of the bar and make the party names normal 
# (democrat), average number of children instead of number of children and 
# colors per column

#remarriage comparison between dems and reps
summary_spouse_remarriage <- new_dat |>
  group_by(party)|>
  summarise(avg_n_remarriage = mean(remarriage, na.rm = TRUE))

#average children per party
summary_kids_non <- new_dat |>
  group_by(party)|>
  summarise(avg_n_children_non = mean(n_children, na.rm = TRUE))

#remarriage comparison between dems and reps
summary_spouse_remarriage <- new_dat |>
  group_by(party)|>
  summarise(avg_n_remarriage = mean(remarriage, na.rm = TRUE))

#bar graphs for remarriage
dem_rep_bar <-  summary_spouse_remarriage |> 
  ggplot(mapping = aes(x = party, y = avg_n_remarriage, fill = party)) +
  geom_col(position = "dodge") + scale_fill_manual(values=c("#1E1BE3","#E31B1B"))+
  labs(x = "Political Parties",
  y = "Average Number of Remarriages") +
  theme_classic() + scale_x_discrete(labels=c("Democratic Party", "Republican Party")) + 
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) + theme(legend.position="none") + geom_bar(stat="identity") + geom_text(aes(label=round(avg_n_remarriage, 2)), vjust=-0.5)
print(dem_rep_bar)

ggsave('bar_plot_spouse.pdf', plot = dem_rep_bar, width = 14 , height = 14, units = "cm")
#bar graph for kid number
dem_rep_bar_kid <-  summary_kids_non |> 
  ggplot(mapping = aes(x = party, y = avg_n_children_non, fill=party)) +
  geom_col(position = "dodge") + scale_fill_manual(values=c("#1E1BE3","#E31B1B"))+
  labs(x = "Political Parties",
  y = "Average Number of Children") +
  theme_classic() + scale_x_discrete(labels=c("Democratic Party", "Republican Party")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) + theme(legend.position="none") + geom_bar(stat="identity") + geom_text(aes(label=round(avg_n_children_non, 2)), vjust=-0.5)
print(dem_rep_bar_kid)

ggsave('bar_plot_kid.pdf', plot = dem_rep_bar_kid, width = 14 , height = 14, units = "cm")
