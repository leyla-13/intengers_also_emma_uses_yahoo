## this page is for the the independent samples
## t test for dem/rep child and wife differences

library(stats)
library(tidyverse)
library(car)

prefilter <- read_csv('politicians_data_all.csv')
#filtering data more
## data <- filter()


#col_names are: 'name', 'party' 'n_spouses', 'n_children', 'religion', 'birthyear'
data <- filter(prefilter, Party == "Democratic Party (United States)"| Party == "Republican Party (United States)") |>
  rename("name" = Name, "party" = Party ,"n_spouses"  = n_Spouses, "n_children" = n_Children, "religion"  = Religion, "birth_year" =  Birthyear) |>
  mutate(n_spouses = as.numeric(n_spouses), n_children = as.numeric(n_children), birth_year = as.numeric(birth_year)) 

##
distinct(data, religion) |>
  print(n = 200) 



## spouse and kid data present
dat_spouse <- filter(data, n_spouses > 0) |>
  mutate(n_spouses = as.numeric(n_spouses), n_children = as.numeric(n_children), birth_year = as.numeric(birth_year)) 

# of divorces could be good
# look at distribution of zeros as well



# find out how to seperate into groups
# this one is for wives

# spouses_dem <- filter(data, party == "democratic") |>
#   filter(n_spouses > 0)

# spouses_rep <- filter(data, party == "republican")


# # leveneTest(n_spouses ~ party, data = data)

# t.test(dems$n_spouses, spouses_rep$n_children, 
# alternative = "greater",
# var.equal=TRUE, data = data)





# this one is for kids

## bar plot dem-rep difference

summary_spouse_non <- dat_spouse |>
  group_by(party)|>
  summarise(avg_n_spouses_non = mean(n_spouses, na.rm = TRUE))

summary_kids_non <- dat_spouse |>
  group_by(party)|>
  summarise(avg_n_children_non = mean(n_children, na.rm = TRUE))

summary_spouse <- data |>
  group_by(party)|>
  summarise(avg_n_spouses = mean(n_spouses, na.rm = TRUE))

dem_rep_bar <-  summary_spouse_non |> 
  ggplot(mapping = aes(x = party, y = avg_n_spouses_non)) +
  geom_col(position = "dodge", color = "#000000ff", fill = "#00000082") +
  labs(x = "Party",
  y = "Number of Spouses") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
print(dem_rep_bar)

ggsave('bar_plot_spouse.pdf', plot = dem_rep_bar, width = 8 , height = 10, units = "cm")


dem_rep_bar_kid <-  summary_kids_non |> 
  ggplot(mapping = aes(x = party, y = avg_n_children_non)) +
  geom_col(position = "dodge", color = "#000000ff", fill = "#00000082") +
  labs(x = "Party",
  y = "Number of Children") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
print(dem_rep_bar_kid)

ggsave('bar_plot_kid.pdf', plot = dem_rep_bar_kid, width = 8 , height = 10, units = "cm")

# year_reg <- 
#   ggplot(dat_spouse, aes(x=birth_year,y= n_spouses, color=party))+
#   geom_point(alpha=1)+
#   geom_smooth(method= "lm")+
#   scale_color_discrete(labels=c("Democratic Party","Republican Party"))+
#   theme_minimal() 
# print(year_reg)
