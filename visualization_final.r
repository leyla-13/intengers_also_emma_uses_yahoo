 library(tidyverse)


prefilter <- read_csv('politicians_data_all.csv')
#filtering data more
## data <- filter()


#col_names are: 'name', 'party' 'n_spouses', 'n_children', 'religion', 'birthyear'
data <- filter(prefilter, Party == "Democratic Party (United States)"| Party == "Republican Party (United States)") |>
  rename("name" = Name, "party" = Party ,"n_spouses"  = n_Spouses, "n_children" = n_Children, "religion"  = Religion, "birth_year" =  Birthyear) |>
  mutate(n_spouses = as.numeric(n_spouses), n_children = as.numeric(n_children), birth_year = as.numeric(birth_year)) 

data_spouse_over0 <- filter(data, n_spouses>0)

data_spouse_over0_birthyear <- filter(data_spouse_over0, birth_year>0)

data_spouse_over0_birthyear_party <- group_by(data_spouse_over0_birthyear, party, birth_year)|>
  summarise(avg_spouses_year=mean(n_spouses))

ggplot(data_spouse_over0_birthyear_party, aes(x=birth_year, y=avg_spouses_year, color=party)) +
  geom_point()+
  scale_color_manual(values=c("Democratic Party (United States)"="#0008ffff", "Republican Party (United States)"="#ff0000ff"))+
  labs(x="Birth year",
      y="Number of spouses",
    color="Political party")