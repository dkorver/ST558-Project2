
library(tidyverse)
library(gtsummary)
# Read in data
nfl_plays <- read_csv("/Users/danekorver/Documents/Fall2024 ST558/scratch/NFL Play by Play 2009-2018 (v5).csv")
nfl_plays$year<-substr(nfl_plays$game_date,1,4)
nfl_plays$order<-1
nfl_plays <- nfl_plays %>% 
  labelled::set_variable_labels(play_type = "Type of Play",
                                field_goal_result = "Field Goal Result")

totals <- nfl_plays %>% 
  filter(play_type=="run" | play_type=="pass") %>%
  group_by(game_id,posteam_type,play_type) %>%
  summarise(total=sum(yards_gained))
#create a new variable that concatenates posteam_type and play_type 
totals$category <- paste(totals$posteam_type,totals$play_type,sep='_')
#transform to wide format
totals_wide <- totals %>%
  pivot_wider(id_cols=game_id,names_from = category, values_from = total)

#last play of the game
final_score <- nfl_plays %>% 
  select(game_id,total_home_score,total_away_score,posteam_type,play_type,field_goal_result,kick_distance,posteam_type) %>%
  group_by(game_id) %>%
  slice_tail(n = 1) %>%
  ungroup()
#Did the Home team win the game? 1=yes, 0=no
final_score$home_team_won <- ifelse(final_score$total_home_score>final_score$total_away_score,1,0)
#Score differential
final_score$score_diff <- final_score$total_home_score-final_score$total_away_score
#merge totals_wide and final_score 
totals_wide2 <- left_join(totals_wide,final_score,join_by(game_id)) 


nfl_plays %>% 
  count(play_type) %>%
  mutate(percent=n*100/sum(n))

nfl_plays %>% 
  select(play_type) %>%
  labelled::set_variable_labels(play_type = "Type of Play") %>%
  tbl_summary()

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

nfl_plays %>% 
  filter(play_type=="field_goal") %>%
  count(qtr,down) %>%
  mutate(percent=n*100/sum(n))

fg_made <- nfl_plays %>% 
  filter(play_type=="field_goal" & field_goal_result=="made")

ggplot(data=fg_made,aes(x=kick_distance)) +
  geom_bar(stat="count")

kickoff <- nfl_plays %>% 
  filter(play_type=="kickoff" & return_yards>0)

ggplot(data=kickoff,aes(x=return_yards)) +
  geom_bar(stat="count")
summary(kickoff$return_yards)



final_score %>% 
  filter(!is.na(play_type)) %>%
  count(play_type) %>%
  mutate(percent=n*100/sum(n))

final_score$home_team_won <- ifelse(final_score$total_home_score>final_score$total_away_score,1,0)
final_score$score_diff <- final_score$total_home_score-final_score$total_away_score
totals$category <- paste(totals$posteam_type,totals$play_type,sep='_')

totals_wide <- totals %>%
  pivot_wider(id_cols=game_id,names_from = category, values_from = total)

totals_wide2 <- left_join(totals_wide,final_score, join_by(game_id)) 

totals_home <- totals_wide2 %>% filter(home_team_won==0) 
summary(totals_wide2$home_team_won)
summary(totals_home$score_diff)
ggplot(totals_home, aes(x=home_pass, y=away_pass, color=score_diff)) + geom_point()


summary(final_score_fg$kick_distance)
ggplot(data=final_score_fg,aes(x=kick_distance)) +
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

ggplot(final_score_fg, aes(x=kick_distance, y=score_diff, color=posteam_type)) + geom_point()
ggplot(final_score_fg, aes(x=score_diff, y=kick_distance, color=posteam_type)) + geom_point()

# Map the time of day to different fill colors
fg_pcts <- final_score %>% 
  filter(play_type=="field_goal") %>%
  count(posteam_type,field_goal_result) %>%
  group_by(posteam_type) %>%
  mutate(percent=n*100/sum(n)) %>%
  ungroup()
  
ggplot(data=fg_pcts, aes(x=posteam_type,y=percent,fill=field_goal_result)) +
  geom_bar(stat="identity",position=position_dodge()) +
  labs(title = "Histogram of Field Goals on the last play of the game",
       subtitle = "for the years 2009-2018",
       x = "Kicking team",
       y = "Percent") +
  scale_fill_hue(name="Field Goal Result") + 
  theme_minimal() +
  theme(plot.title=element_text(hjust = 0.5),
        plot.subtitle=element_text(hjust = 0.5),
        axis.line = element_line(colour = "grey50"))+
  scale_y_continuous(limits=c(0,100),breaks=c(0,10,20,30,40,50,60,70,80,90,100))

final_score_fg <- final_score %>% 
  filter(play_type=="field_goal")

fg_pcts2 <- final_score_fg %>% 
  filter(play_type=="field_goal") %>%
  count(kick_distance,score_diff,posteam_type,field_goal_result) %>%
  group_by(score_diff) %>%
  mutate(size=n*100/sum(n))

fg_pcts2$category <- paste(fg_pcts2$posteam_type,fg_pcts2$field_goal_result,sep=' ')

summary(fg_pcts2$n)

ggplot(final_score_fg, aes(x=score_diff,y=kick_distance, color=field_goal_result)) + geom_point()

ggplot(fg_pcts2, aes(x=score_diff,y=kick_distance, size = size, color = category)) +
  geom_point() +
  #scale_size_area(max_size = 5)+
  scale_x_continuous(limits=c(-10,4),breaks=c(-10,-8,-6,-5,-4,-3,-2,-1,0,1,2,3,4))+
  scale_y_continuous(limits=c(0,100),breaks=c(0,10,20,30,40,50,60,70,80,90,100))

kicker_stats <- nfl_plays %>% 
  filter(play_type=="field_goal") %>%
  group_by(kicker_player_name,field_goal_result) %>%
  summarise(total=sum(order)) %>%
  ungroup() %>%
  group_by(kicker_player_name) %>%
  mutate(percent=(total/sum(total))*100)

kicker_stats_made <- kicker_stats %>% 
  filter(field_goal_result=="made")

ggplot(kicker_stats_made, aes(x = percent, y = reorder(kicker_player_name, percent))) +
  geom_point(size = 0.5) +  
  labs(title = "Cleveland Dot Plot of Field Goals 'Made'",
       subtitle = "by Player for the years 2009-2018",
       x = "Percent",
       y = "Kicker") +
  scale_x_continuous(limits=c(0,100),breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
  theme_bw() +
  theme(
    plot.title=element_text(hjust = 0.5),
    plot.subtitle=element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(angle = 10, vjust = 1, size=5)
  )



