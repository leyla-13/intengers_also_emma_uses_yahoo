 library(tidyverse)


prefilter <- read_csv('politicians_data_all.csv')
#filtering data more
## data <- filter()


#col_names are: 'name', 'party' 'n_spouses', 'n_children', 'religion', 'birthyear'
data <- filter(prefilter, Party == "Democratic Party (United States)"| Party == "Republican Party (United States)") |>
  rename("name" = Name, "party" = Party ,"n_spouses"  = n_Spouses, "n_children" = n_Children, "religion"  = Religion, "birth_year" =  Birthyear) |>
  mutate(n_spouses = as.numeric(n_spouses), n_children = as.numeric(n_children), birth_year = as.numeric(birth_year)) 

data_spouse_over0_birthyear_party <- filter(data, n_spouses>0)|>
  filter(birth_year>0)|>
  group_by(party, birth_year)|>
  summarise(avg_spouses_year=mean(n_spouses)) 

ggplot(data_spouse_over0_birthyear_party, aes(x=birth_year, y=avg_spouses_year, color=party)) +
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("Democratic Party (United States)"="#0008ffff", "Republican Party (United States)"="#ff0000ff"))+
  labs(x="Birth year",
      y="Average number of spouses",
    color="Political party")
ggsave("Average number of spouses vs Birthyear per political party.pdf", width=30, height=10, units="cm")

data_children_over0_birthyear_party <- filter(data, n_children>0)|>
  filter(birth_year>0)|>
  group_by(party, birth_year)|>
  summarise(avg_children_year=mean(n_children)) 

ggplot(data_children_over0_birthyear_party, aes(x=birth_year, y=avg_children_year, color=party)) +
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("Democratic Party (United States)"="#0008ffff", "Republican Party (United States)"="#ff0000ff"))+
  labs(x="Birth year",
      y="Average number of children",
    color="Political party")
ggsave("Average number of children vs Birthyear per political party.pdf", width=30, height=10, units="cm")