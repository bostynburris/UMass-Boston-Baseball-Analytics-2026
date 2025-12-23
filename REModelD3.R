#### using my previous RE model to create one for entire league and use at division 3 level

install.packages("retrosheet")
install.packages("baseballr")
library(retrosheet)
library(baseballr)
library(Lahman)
library(tidyverse)

library(readr)
BOSevent <- read_csv("csv_output/2025BOS.csv")
View(BOSevent)

View(BOSevent)

# Vector of Retrosheet .EVA event file column names
event_cols <- c(
  "game_id", "visiting_team", "inning", "batting_team", "outs", "balls", "strikes",
  "pitch_sequence", "vis_score", "home_score", "batter", "batter_hand", "res_batter", 
  "res_batter_hand", "pitcher", "pitcher_hand", "res_pitcher", "res_pitcher_hand", 
  "catcher", "first_base", "second_base", "third_base", "shortstop", "left_field", 
  "center_field", "right_field", "first_runner", "second_runner", "third_runner", 
  "event_text", "leadoff_flag", "pinchhit_flag", "defensive_position", "lineup_position", 
  "event_type", "batter_event_flag", "ab_flag", "hit_value", "SH_flag", "SF_flag", 
  "outs_on_play", "double_play_flag", "triple_play_flag", "RBI_on_play", "wild_pitch_flag", 
  "passed_ball_flag", "fielded_by", "batted_ball_type", "bunt_flag", "foul_flag", 
  "hit_location", "num_errors", "first_error_player", "first_error_type", 
  "second_error_player", "second_error_type", "third_error_player", "third_error_type", 
  "batter_dest", "runner1_dest", "runner2_dest", "runner3_dest", "play_on_batter", 
  "play_on_runner1", "play_on_runner2", "play_on_runner3", "SB_runner1_flag", 
  "SB_runner2_flag", "SB_runner3_flag", "CS_runner1_flag", "CS_runner2_flag", 
  "CS_runner3_flag", "PO_runner1_flag", "PO_runner2_flag", "PO_runner3_flag", 
  "resp_pitcher_runner1", "resp_pitcher_runner2", "resp_pitcher_runner3", "new_game_flag", 
  "end_game_flag", "pinch_runner1_flag", "pinch_runner2_flag", "pinch_runner3_flag", 
  "removed_runner1_id", "removed_runner2_id", "removed_runner3_id", "removed_batter_id", 
  "removed_batter_fielding_position", "fielder_first_putout", "fielder_second_putout", 
  "fielder_third_putout", "fielder_first_assist", "fielder_second_assist", 
  "fielder_third_assist", "fielder_fourth_assist", "fielder_fifth_assist", "event_num"
)

colnames(BOSevent) <- event_cols
BOSevent <- BOSevent[, -c(12, 13, 14, 16, 17, 18)]


BOSevent <- BOSevent %>%
  mutate(
    base_state = paste0(
      ifelse(first_runner != '', 1, 0),
      ifelse(second_runner != '', 1, 0),
      ifelse(third_runner != '', 1, 0)
    )
  )

head(BOSevent)

BOSevent <- BOSevent %>%
  rowwise() %>%
  mutate(
    runs_scored = sum(
      batter_dest %in% 4:5,
      runner1_dest %in% 4:5,
      runner2_dest %in% 4:5,
      runner3_dest %in% 4:5
    )
  )

View(BOSevent)

BOSevent <- BOSevent %>%
  filter(batting_team == "1")
BOSevent <- BOSevent %>%
  filter(!(visiting_team != "BOS" & inning == 9 & outs < 3 & end_game_flag == 1))

run_expectancyBOS2025home <- BOSevent %>%
  group_by(outs, base_state) %>%
  summarize(
    avg_runs = mean(runs_scored, na.rm = TRUE),
    num_plays = n(),
    .groups = "drop"
  ) %>%
  arrange(outs, base_state)



View(run_expectancyBOS2025home)

#### shows the expected number of runs scored in 
#### each of the 24 possibilities of base state and outs

##############################################################################################################

library(readr)
library(tidyverse)

event2025 <- read.csv("2025plays.csv")
View(event2025)

event2025 <- event2025 %>%
  mutate(
    base_state = paste0(
      ifelse(br1_pre != '', 1, 0),
      ifelse(br2_pre != '', 1, 0),
      ifelse(br3_pre != '', 1, 0)
    )
  )

event2025 <- event2025 %>%
  mutate(
    runs_scored_on_play = rowSums(
      cbind(
      run_b != "",
      run1 != "",
      run2 != "",
      run3 != ""
    ),
    na.rm = TRUE
  )
)
View(event2025)

event2025 <- event2025 %>%
  mutate(
    end_game = case_when(
      inning >= 9 & top_bot == 1 & (score_h + runs) > score_v ~ TRUE,
      outs_post == 3 & top_bot == 0 & inning >= 9 & score_h > score_v ~ TRUE,
      outs_post == 3 & inning >= 9 & top_bot == 1 & score_v > score_h ~ TRUE,
      outs_post == 3 & top_bot == 1 & inning >= 9 & score_h > score_v ~ TRUE,
      TRUE ~ FALSE
    )
  )


run_expectancyMLB2025 <- event2025 %>%
  filter(outs_pre < 3) %>%
  group_by(outs_pre, base_state) %>%
  summarize(
    avg_runs = mean(runs, na.rm = TRUE),
    num_plays = n(),
    .groups = "drop"
  ) %>%
  arrange(outs_pre, base_state)

View(run_expectancyMLB2025)

#### run expectancy model above shows expected number of runs to score on that specific play

#### next model will show how many runs are expected to score from that point to
#### the rest of the inning

##############################################################################################################


event2025 <- event2025 %>%
  group_by(gid, inning) %>%
  mutate(
    runs_remaining = rev(cumsum(rev(runs))) 
  ) %>%
  ungroup()

run_expectancy_inning_MLB2025 <- event2025 %>%
  filter(outs_pre < 3) %>%
  group_by(outs_pre, base_state) %>%
  summarize(
    avg_runs = mean(runs_remaining, na.rm = TRUE),
    num_plays = n(),
    .groups = "drop"
  ) %>%
  arrange(outs_pre, base_state)

View(run_expectancy_inning_MLB2025)

run_expectancy_heatmap <- run_expectancy_inning_MLB2025 %>%
  mutate(
    base_state = factor(base_state, levels = c("000","001","010","011","100","101","110","111")),
    outs_pre = factor(outs_pre, levels = c(0,1,2))
  )

ggplot(run_expectancy_heatmap, aes(x = base_state, y = outs_pre, fill = avg_runs)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(avg_runs, 2)), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(
    title = "Run Expectancy by Base State and Outs (Rest of Inning)",
    x = "Base State (1=Occupied, 0=Empty; 1st-2nd-3rd)",
    y = "Outs",
    fill = "Expected Runs"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(face = "bold")
  )
