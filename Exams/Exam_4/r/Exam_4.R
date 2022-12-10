library(tidyverse)
library(janitor)

# Task I. Read the cleaned_covid_data.csv file into an R data frame. (20 pts)
df<-read_csv("./data/cleaned_covid_data.csv") %>% clean_names()

# Task II. Subset the data set to just show states that begin with “A” and save this as an object called A_states. (20 pts)
A_states<-df %>%
  filter(grepl("^A",province_state))


#Task III. Create a plot of that subset showing Deaths over time, with a separate facet for each state. (20 pts)

  ggplot(aes(x=last_update,y=deaths))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(~province_state, scales = "free")+
  theme_bw()

# Task IV. (Back to the full dataset) Find the “peak” of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate. (20 pts)

state_max_fatality_rate<-df %>%
  group_by(province_state) %>%
  summarise(maximum_fatality_ratio = max(case_fatality_ratio,na.rm = TRUE)) %>%
  arrange(desc(maximum_fatality_ratio))

#Task V. Use that new data frame from task IV to create another plot. (20 pts)

state_max_fatality_rate %>%
  mutate(province_state = factor(province_state,levels = province_state)) %>%
  ggplot(aes(x=province_state, y=maximum_fatality_ratio))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))

# Task (BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the entire US over time
df %>%
  group_by(last_update) %>%
  summarise(totaldeaths = sum(deaths, na.rm = TRUE)) %>%
  ggplot(aes(x=last_update, y =totaldeaths)) +
  geom_point()+
  theme_bw()

