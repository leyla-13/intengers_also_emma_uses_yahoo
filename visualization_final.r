 library(tidyverse)


data <- read_csv('new_cleaner_beautiful_data.csv')
#filtering data more
## data <- filter()


#col_names are: 'Name', 'Party' 'n_Spouses', 'n_Children', 'Religion', 'Birthyear'


ggplot(data, aes(x=generation, y=n_spouses, fill=party)) +
  geom_bar(stat="summary", fun=mean, position="dodge")+
  scale_fill_manual(values=c("Democratic Party (United States)"="#1E1BE3", "Republican Party (United States)"="#E31B1B"))+
  labs(x="Generation",
      y="Average number of spouses",
      fill="Political party") + scale_x_discrete(labels=c("Baby Boomers", "Generation X", "Generation Y", "Greatest Generation", "Silent Generation"))
  
ggsave("Average number of spouses vs Generation per political party.pdf", width=30, height=10, units="cm")

# ggplot(data_children_over0_birthyear_party, aes(x=birth_year, y=avg_children_year, color=party)) +
#   geom_point()+
#   geom_line()+
#   scale_color_manual(values=c("Democratic Party (United States)"="#1E1BE3", "Republican Party (United States)"="#E31B1B"))+
#   labs(x="Birth year",
#       y="Average number of children",
#     color="Political party")
# ggsave("Average number of children vs Birthyear per political party.pdf", width=30, height=10, units="cm")