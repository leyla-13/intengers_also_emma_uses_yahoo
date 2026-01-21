## this page is for the the independent samples
## t test for dem/rep child and wife differences

library(stats)
library(tidyverse)
library(car)


prefilter <- read_csv('politicians_data_all.csv')
#filtering data more
## data <- filter()


## filtration filtered everything that:
  # wasnt a party with the name democratic or republican, some data said stuff like liutenant
  # combined religions into subcategories

data <- filter(prefilter, Party == "Democratic Party (United States)"| Party == "Republican Party (United States)") |>
  rename("name" = Name, "party" = Party ,"n_spouses"  = n_Spouses, "n_children" = n_Children, "religion"  = Religion, "birth_year" =  Birthyear) |>
  mutate(n_spouses = as.numeric(n_spouses), n_children = as.numeric(n_children), birth_year = as.numeric(birth_year)) |>
  mutate(religion = case_match(religion,
      "Universalism" ~ "Progressive Christian",
      "United Church of Christ" ~ "Progressive Christian",
      "Evangelical Lutheran Church in America" ~ "Progressive Christian",
      "United Methodist Church" ~ "Progressive Christian",
      "Presbyterian Church in the United States of America" ~ "Progressive Christian",
      "Presbyterian Church in the United States" ~ "Progressive Christian",
      "Episcopal Church (United States) " ~ "Progressive Christian",
      "Unitarian Universalism" ~ "Progressive Christian",
      "Christian Church (Disciples of Christ)" ~ "Progressive Christian",
      "['United Methodist Church' " ~ "Progressive Christian",
      "The Salvation Army" ~ "Progressive Christian",
      "Presbyterian Church (USA) " ~ "Progressive Christian",
      "Seventh-day Adventist Church" ~ "Conservative Christian",
      "Congregational church" ~ "Conservative Christian",
      "Christian churches and churches of Christ" ~ "Conservative Christian",
      "Church of Christ" ~ "Conservative Christian",
      "Congregational Christian Church in American Samoa" ~ "Conservative Christian",
      "Christian fundamentalism " ~ "Conservative Christian",
      "Lutheran Church–Missouri Synod" ~ "Conservative Christian",
      "Evangelicalism" ~ "Conservative Christian",
      "Churches of Christ" ~ "Conservative Christian",
      "Church of the Nazarene" ~ "Conservative Christian",
      "Christian and Missionary Alliance" ~ "Conservative Christian",
      "The Church of Jesus Christ of Latter-day Saints" ~ "Conservative Christian",
      "Southern Baptist Convention " ~ "Conservative Christian",
      "Presbyterian Church in America" ~ "Conservative Christian",
      "Anglican Church in North America" ~ "Conservative Christian",
      "Greek Orthodox Church" ~ "Conservative Christian",
      "Eastern Orthodox Church" ~ "Conservative Christian",
      "Holiness movement " ~ "Conservative Christian",
      "Missionary Baptists" ~ "Conservative Christian",
      "Quakers" ~ "Conservative Christian",
      "African Methodist Episcopal Church" ~ "Conservative Christian",
      "Christian" ~ "Undefined Christian",
      "Christianity" ~ "Undefined Christian",
      "['Christianity'" ~ "Undefined Christian",
      "Protestantism" ~ "Undefined Christian",
      "Lutheranism" ~ "Undefined Christian",
      "Calvinism" ~ "Undefined Christian",
      "['Calvinism'" ~ "Undefined Christian",
      "Baptists" ~ "Undefined Christian",
      "['Baptists' " ~ "Undefined Christian",
      "Methodism " ~ "Undefined Christian",
      "Presbyterianism" ~ "Undefined Christian",
      "Anglicanism" ~ "Undefined Christian",
      "Catholic Church" ~ "Undefined Christian",
      "Catholicismx" ~ "Undefined Christian",
      "['Catholic Church' " ~ "Undefined Christian",
      "Nondenominational Christianity  " ~ "Undefined Christian",
      "New Hope Christian Fellowship " ~ "Undefined Christian",
      "Religious views of Abraham Lincoln " ~ "Undefined Christian",
      "Christian Reformed Church in North America" ~ "Undefined Christian",
      "Christian Science" ~ "Undefined Christian",
      "Islam" ~ "Non Christian",
      "Judaism" ~ "Non Christian",
      "Jews" ~ "Non Christian",
      "Reform Judaism " ~ "Non Christian",
      "Conservative Judaism" ~ "Non Christian",
      "Sephardi Jews" ~ "Non Christian",
      "American Jews " ~ "Non Christian",
      "['Agnosticism'" ~ "Non Christian",
      "['Scientology'" ~ "Non Christian",
      "Unitarianism" ~ "Non Christian"
    )
  )

distinct(data, religion) |>
  print(n = 200) 


## filtered for data that has a spouses listed (over 0) and added a remarriage column
dat_spouse <- filter(data,  n_spouses > 0) |>
  mutate(n_spouses = as.numeric(n_spouses), n_children = as.numeric(n_children), 
  birth_year = as.numeric(birth_year)) |>
  mutate(remarriage = n_spouses - 1) |>
  filter(birth_year > 1901)

## birth year grouping by generation
generation_making <- function(birth_year) {
  if (is.na(birth_year)) {
    return(NA)
  }
  if (birth_year <= 1927) {
    return("greatest_generation")
  } else if (birth_year <= 1945) {
    return("silent_generation")
  } else if (birth_year <= 1964) {
    return("baby_boomers")
  } else if (birth_year <= 1980) {
    return("generation_x")
  }
  else {
    return("generation_y")
  }
}



dat_spouse <- dat_spouse |>
  mutate(generation = sapply(birth_year, generation_making))


# distinct(dat_spouse, religion) |>
#   print(n = 200) 

write_csv(dat_spouse, "new_cleaner_beautiful_data.csv")

#looking at the zero distribution for children

distribution_child <-  dat_spouse |> 
  ggplot(mapping = aes(x = n_children, fill = party)) +
  geom_bar(position = "dodge") +
  labs(x = "children count") +
  theme_classic()

#looking at the zero distribution for remarriage
distribution_spouse <-  dat_spouse |> 
  ggplot(mapping = aes(x = n_spouses, fill = party)) +
  geom_bar(position = "dodge") +
  labs(x = "remarriage count") +
  theme_classic()

print(distribution_child)
print(distribution_spouse)




#variation analysis

sum_rep <- dat_spouse |>
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
summary_spouse_remarriage <- dat_spouse |>
  group_by(party)|>
  summarise(avg_n_remarriage = mean(remarriage, na.rm = TRUE))

#average children per party
summary_kids_non <- dat_spouse |>
  group_by(party)|>
  summarise(avg_n_children_non = mean(n_children, na.rm = TRUE))

#remarriage comparison between dems and reps
summary_spouse_remarriage <- dat_spouse |>
  group_by(party)|>
  summarise(avg_n_remarriage = mean(remarriage, na.rm = TRUE))

#bar graphs for remarriage
dem_rep_bar <-  summary_spouse_remarriage |> 
  ggplot(mapping = aes(x = party, y = avg_n_remarriage)) +
  geom_col(position = "dodge", color = "#000000ff", fill = "#00000082") +
  labs(x = "Party",
  y = "Number of Spouses") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
print(dem_rep_bar)

ggsave('bar_plot_spouse.pdf', plot = dem_rep_bar, width = 8 , height = 10, units = "cm")

#bar graph for kid number
dem_rep_bar_kid <-  summary_kids_non |> 
  ggplot(mapping = aes(x = party, y = avg_n_children_non)) +
  geom_col(position = "dodge", color = "#000000ff", fill = "#00000082") +
  labs(x = "Party",
  y = "Number of Children") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
print(dem_rep_bar_kid)

ggsave('bar_plot_kid.pdf', plot = dem_rep_bar_kid, width = 8 , height = 10, units = "cm")


