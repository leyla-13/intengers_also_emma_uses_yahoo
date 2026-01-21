 library(tidyverse)


data <- read_csv('new_cleaner_beautiful_data.csv')|>
  mutate(generation=fct_relevel(generation, c("greatest_generation", "silent_generation", "baby_boomers", "generation_x", "generation_y")))

ggplot(data, aes(x=generation, y=n_spouses, fill=party)) +
  geom_bar(stat="summary", fun=mean, position="dodge")+
  scale_fill_manual(values=c("Democratic Party (United States)"="#1E1BE3", "Republican Party (United States)"="#E31B1B"))+
  labs(x="Generation",
      y="Average number of spouses",
      fill="Political party")+
  scale_x_discrete(labels=c("Greatest Generation","Silent Generation", "Baby Boomers", "Generation X", "Generation Y"))
  
ggsave("Average number of spouses vs Generation per political party.pdf", width=30, height=10, units="cm")