#install.packages("nflseedR")
library(nflseedR)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
options(digits = 3)
options(warn = -1)
library(ggimage)
library(RCurl)
library(ggrepel)
library(gganimate)
library(ggpmisc)

games <- nflseedR::load_sharpe_games()

glimpse(games)

#### calculate record ATS #####
sub_games<-games %>%
  filter(game_type == "REG") %>%
  select(game_id,season,week,away_team,away_score,home_team,home_score,result,spread_line) %>%
  mutate(
    favor = case_when(
      spread_line < 0 ~ "AWAY",
      spread_line > 0 ~ "HOME",
      spread_line == 0 ~ "EVEN",
      TRUE ~ as.character(NA)
    ),
    win = case_when(
      result > 0 ~ "HOME",
      result < 0 ~ "AWAY",
      result == 0 ~ "TIE",
      TRUE ~ as.character(NA)
    ),
    cover = case_when(
      ### result & spread_line greater than 0 scenario
      result > 0 & spread_line > 0 & result > spread_line ~ "HOME",
      result > 0 & spread_line > 0 & result < spread_line ~ "AWAY",
      ### 
      result > 0 & spread_line < 0 & result > spread_line ~ "HOME",
      ### negative 
      result < 0 & spread_line < 0 & result < spread_line ~ "AWAY",
      result < 0 & spread_line < 0 & result > spread_line ~ "HOME",
      ###
      result < 0 & spread_line > 0 & result < spread_line ~ "AWAY",
      ## equal
      spread_line == 0 & result > spread_line ~ "HOME",
      spread_line == 0 & result < spread_line ~ "AWAY",
      ### result = 0
      result == 0 & spread_line > 0 ~ "AWAY",
      result == 0 & spread_line < 0 ~ "HOME",
      ### PUSH
      result == spread_line ~ "PUSH",
      TRUE ~ as.character(NA)
    )
)


home<-sub_games %>%
  select(game_id,season,week,home_team,home_score,result,spread_line,favor,win,cover) %>%
  mutate(favor_win = ifelse(favor == "HOME",1,0),
         favor_lose = ifelse(favor == "AWAY",1,0),
         favor_even = ifelse(favor == "EVEN",1,0),
         win = ifelse(win == "HOME",1,0),
         lose = ifelse(win == "AWAY",1,0),
         tie = ifelse(win == "TIE",1,0),
         cover_win = ifelse(cover == "HOME",1,0),
         cover_lose = ifelse(cover == "AWAY",1,0),
         cover_push = ifelse(cover == "PUSH",1,0),
         side = "HOME") %>%
  group_by(season,home_team,side) %>%
  rename(tm = home_team) %>%
  summarize(games = n(),
            favor_win = sum(favor_win),
            favor_lose = sum(favor_lose),
            favor_even = sum(favor_even),
            win = sum(win),
            lose = sum(lose),
            tie = sum(tie),
            cover_win = sum(cover_win),
            cover_lose = sum(cover_lose),
            cover_push = sum(cover_push))
  
      
away<-sub_games %>%
  select(game_id,season,week,away_team,away_score,result,spread_line,favor,win,cover) %>%
  mutate(favor_win = ifelse(favor == "AWAY",1,0),
         favor_lose = ifelse(favor == "HOME",1,0),
         favor_even = ifelse(favor == "EVEN",1,0),
         win = ifelse(win == "AWAY",1,0),
         lose = ifelse(win == "HOME",1,0),
         tie = ifelse(win == "TIE",1,0),
         cover_win = ifelse(cover == "AWAY",1,0),
         cover_lose = ifelse(cover == "HOME",1,0),
         cover_push = ifelse(cover == "PUSH",1,0),
         side = "AWAY") %>%
  group_by(season,away_team,side) %>%
  rename(tm = away_team) %>%
  summarize(games = n(),
            favor_win = sum(favor_win),
            favor_lose = sum(favor_lose),
            favor_even = sum(favor_even),
            win = sum(win),
            lose = sum(lose),
            tie = sum(tie),
            cover_win = sum(cover_win),
            cover_lose = sum(cover_lose),
            cover_push = sum(cover_push))

tot<-home %>%
  union(away) %>%
  mutate(
    tm = case_when(
      tm == "STL"  ~ "LA",
      tm == "SD" ~ "LAC",
      tm == "OAK" ~ "LV",
      TRUE ~ as.character(tm))) %>%
  group_by(season,tm) %>%
  summarize(games = sum(games),
            favor_win = sum(favor_win),
            favor_lose = sum(favor_lose),
            favor_even = sum(favor_even),
            win = sum(win),
            lose = sum(lose),
            tie = sum(tie),
            cover_win = sum(cover_win),
            cover_lose = sum(cover_lose),
            cover_push = sum(cover_push)) %>%
  mutate(favor_win_pct = favor_win / games,
         win_pct = win / games,
         cover_win_pct = cover_win / games) %>%
  ungroup() %>%         
  arrange(tm,season) %>%
  mutate(favor_win_pct_prev = ifelse(lag(tm) == tm,lag(favor_win_pct),NA),
         win_pct_prev = ifelse(lag(tm) == tm,lag(win_pct),NA),
         cover_win_pct_prev = ifelse(lag(tm) == tm,lag(cover_win_pct),NA),
         tm_yr = paste(tm,season,sep="-")
         )
         

### adding logos
url.logo <- getURL("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
df.logos <- read.csv(text = url.logo)

tot<-tot %>%
  left_join(df.logos,by=c("tm" = "team_code"))

final<-na.omit(tot)
  
final<-final %>%
  mutate(win_pct_delta = win_pct - win_pct_prev)

#### 


### check Detroit Lion 2021 to see why they were 11-6 ATS
final %>%
  filter(season >= "2016") %>%
ggplot(aes(x=win_pct_delta,y=cover_win_pct_prev)) +
         geom_point() +
  #geom_text(aes(x=win_pct_delta,y=cover_win_pct_prev,label = season),size = 3,nudge_y = -0.02,color="blue") +
  geom_image(aes(image = url), size = 0.1,check_overlap=TRUE) + 
  geom_hline(aes(yintercept = 0.5), lty = 2, col = "red", alpha = 0.5) + 
  geom_vline(aes(xintercept = 0.0), lty = 2, col = "red", alpha = 0.5) +
  stat_smooth(method = "lm",geom="line",alpha = 0.5,se=FALSE,color="grey",size=1) +
  labs(title = "Does Record Against the Spread build momentum for next season?",
       subtitle = 'Season 2016 - 2021',
       x = "Winning Percentage YoY +/-",
       y = "Previous Season Record ATS", 
      caption = "Viz: @QuinnsWisdom | Data: nflseedR") +
  facet_wrap(~season) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 7),
    axis.text = element_text(size=7),
    plot.title = element_text(size = 12,hjust=0.5),
    plot.subtitle = element_text(size=9,hjust=0.5)
  ) +
  stat_poly_eq(aes(label = ..rr.label..),size = 2,label.x=0.4)

ggsave("WinPct_vs_ATS.png")




#animate(plot)
#anim_save("season_change.gif")


#### look at regression, R^2, 



##### how does that related ######