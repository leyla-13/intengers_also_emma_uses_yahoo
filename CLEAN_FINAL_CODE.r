##FINAL R CODE FOR INTENGERS FINAL PAPER##

#PART 1 - FILTRATION
## please install these packages if they are not already
library(stats)
library(tidyverse)
library(ggplot2)
library(jtools)
library(dplyr)

#getting politian data through python filtration

prefilter <- read_csv('politicians_data_all.csv')


# filtration filtered everything that:
  # wasnt a party with the name democratic or republican, some data had info like liutenant
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


# filtered for data that has a spouses listed (over 0) and added a remarriage column
dat_spouse <- filter(data,  n_spouses > 0) |>
  mutate(n_spouses = as.numeric(n_spouses), n_children = as.numeric(n_children), 
  birth_year = as.numeric(birth_year)) |>
  mutate(remarriage = n_spouses - 1) |>
  filter(birth_year > 1901)

# making a function for birth year grouping by generation
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


# activated function to make a new column

dat_spouse <- dat_spouse |>
  mutate(generation = sapply(birth_year, generation_making))

# wrote the cleaned data into a new csv file
write_csv(dat_spouse, "new_cleaner_beautiful_data.csv")

## PART 2 - VISUALIZATION

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

ggsave('bar_plot_spouse.pdf', plot = dem_rep_bar, width = 10 , height = 12, units = "cm")


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
ggsave("Percentage of politicians that remarried vs Religion per political party.pdf", width=30, height=10, units="cm")


# visalizing relative remarriage grouped by party per generation
generation_remarriage <- new_dat |>
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
 
ggsave("Percentage of politicians that remarried vs Generation per political party.pdf", plot = generation_remarriage, width=30, height=10, units="cm")
