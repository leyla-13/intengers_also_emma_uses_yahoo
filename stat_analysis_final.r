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

data <- 

# find out how to seperate into groups
# this one is for wives

# spouses_dem <- filter(data, party == "democratic") |>
#   filter(n_spouses > 0)

# spouses_rep <- filter(data, party == "republican")


# # leveneTest(n_spouses ~ party, data = data)

# t.test(dems$n_spouses, spouses_rep$n_children, 
# alternative = "greater",
# var.equal=TRUE, data = data)



print(wives_t_test)

# this one is for kids

## bar plot dem-rep difference

summary_spouse_non <- dat_spouse |>
  group_by(party)|>
  summarise(avg_n_spouses_non = mean(n_spouses, na.rm = TRUE))

summary_spouse <- data |>
  group_by(party)|>
  summarise(avg_n_spouses = mean(n_spouses, na.rm = TRUE))

dem_rep_bar <-  data |> 
  ggplot(mapping = aes(x = party, y = avg_n_spouses)) +
  geom_col(position = "dodge", color = "#000000ff", fill = "#f68cc8ff") +
  labs(x = "Grant Status",
  y = "Graduation Rate") +
  scale_x_discrete(labels = c ("With Grant", "Without Grant")) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_classic() + #i made it minimal for you guys
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
ggsave("grad_rate_grant_status_barplot.pdf", plot = grad_rate_bar, width = 8 , height = 10, units = "cm")
