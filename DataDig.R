
library(tidyverse)
# Read in data
nfl_plays <- read_csv("/Users/danekorver/Documents/Fall2024 ST558/scratch/NFL Play by Play 2009-2018 (v5).csv")

nfl_plays %>% 
  count(play_type) %>%
  mutate(percent=n*100/sum(n))

nfl_plays %>% 
  count(play_type) %>%
  mutate(percent=n*100/sum(n))

nfl_plays %>% 
  filter(play_type=="field_goal") %>%
  count(field_goal_result) %>%
  mutate(percent=n*100/sum(n))

fg_made <- nfl_plays %>% 
  filter(play_type=="field_goal" & field_goal_result=="made")

summary(fg_made$kick_distance)
ggplot(data=fg_made,aes(x=kick_distance)) +
  geom_bar(stat="count")

fg_missed <- nfl_plays %>% 
  filter(play_type=="field_goal" & field_goal_result=="missed")

summary(fg_missed$kick_distance)
ggplot(data=fg_missed,aes(x=kick_distance)) +
  geom_bar(stat="count") +
  labs(title = "Histogram of Field Goals 'Made'",
       subtitle = "for the years 2009-2018",
       x = "Kick Distance (in yards)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title=element_text(hjust = 0.5),
        plot.subtitle=element_text(hjust = 0.5),
        axis.line = element_line(colour = "grey50"))+
  scale_x_continuous(breaks = 0:65)

fg_blocked <- nfl_plays %>% 
  filter(play_type=="field_goal" & field_goal_result=="blocked")

summary(fg_blocked$kick_distance)
ggplot(data=fg_blocked,aes(x=kick_distance)) +
  geom_bar(stat="count")



