library(nflfastR)
library(nflreadr)
library(tidyverse)
library(gt)



df<-nflreadr::load_pbp(2018:2021)


unique(df$play_type)

short_rush<-df %>%
  dplyr::filter(play_type == "run") %>%
  dplyr::filter(goal_to_go == 1 |
                  ydstogo <= 3) %>%
  dplyr::group_by(run_gap) %>%
  dplyr::summarize(
    rushes = n(),
    success = sum(success),
    success_rt = round(success / n(),2)
  ) %>%
  mutate(rush_type = "short_rush") %>%
  dplyr::select(-success)

long_rush<-df %>%
  dplyr::filter(play_type == "run") %>%
  dplyr::filter(goal_to_go == 0 &
                  ydstogo >= 3) %>%
  dplyr::group_by(run_gap) %>%
  dplyr::summarize(
    rushes = n(),
    success = sum(success),
    success_rt = round(success / n(),2)
  ) %>%
  mutate(rush_type = "long_rush") %>%
  dplyr::select(-success)
  

rush<-rbind(short_rush,long_rush)

rush %>%
  mutate(
    run_gap = case_when(
      run_gap == "end" ~ "End",
      run_gap == "guard" ~ "Guard",
      run_gap == "tackle" ~ "Tackle",
      run_gap == "NA" ~ "N/A",
      TRUE ~ as.character(NA)
    )
  ) %>%
  pivot_wider(names_from = rush_type,
              values_from = c(rushes,success_rt)) %>%
  gt() %>%
  fmt_percent(
    columns = c(success_rt_short_rush,success_rt_long_rush),
    decimals = 0
  ) %>%
  cols_label(
    run_gap = "Run Gap",
    rushes_short_rush = "Short Rush Atts.",
    rushes_long_rush = "Long Rush Atts.",
    success_rt_short_rush = "Success Rate (Short Rush)",
    success_rt_long_rush = "Success Rate (Long Rush)"
  ) %>% 
  tab_options(
    column_labels.background.color = "white",
    column_labels.font.weight = "bold",
    table.border.top.width = px(3),
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table.border.bottom.width = px(3),
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    column_labels.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    data_row.padding = px(3),
    source_notes.font.size = 12,
    table.font.size = 12,
    heading.align = "left"
  ) %>%
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) %>%
  tab_header(
    title = md("Rushing Success by Run Gap"),
    subtitle = md("Year: 2018 - 2021 | Data: nflfastR | Table: @QuinnsWisdom")
  ) %>%
  tab_footnote(
    footnote = "Short Rush: Goal Line or Yards to Go <4yds",
    locations = cells_column_labels(
      columns = rushes_short_rush
    )) %>%
  tab_footnote(
    footnote = "A measure of running back consistency based on the percentage of carries where the player gains 40% of needed yards on first down, 60% of needed yards on second down, or 100% of needed yards on third or fourth down",
    locations = cells_column_labels(
      columns = success_rt_short_rush
    )) 







