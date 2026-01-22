install.packages("readxl")
library(readxl)
library(tidyverse)
library(baseballr)
  
D32025TeamHittingTotals <- read_excel("D32025TeamHittingTotals.xlsx")
View(D32025TeamHittingTotals)

D32025TeamPitchingTotals <- read_excel("D32025TeamPitchingTotals.xlsx")
View(D32025TeamPitchingTotals)

D3teamtotals <- left_join(D32025TeamHittingTotals, D32025TeamPitchingTotals, by = "Tm")
View(D3teamtotals)

schedule <- c("Bridgewater State Bears", "WPI Engineers", "MIT Engineers", "Lasell Lasers",
              "SUNY Brockport Golden Eagles", "SUNY Brockport Golden Eagles",
              "St. Joseph Blue Jays", "Western New England Golden Bears", "Ohio Northern Polar Bears",
              "Rutgers-Camden Scarlet Raptors", "Rowan Profs", "Mount St. Vincent Dolphins",
              "WestConn Colonials", "WestConn Colonials", "Tufts Jumbos", "Johnson & Wales Wildcats",
              "Keene State Owls", "Keene State Owls",
              "Nichols Bison", "Roger Williams Hawks", "Babson Beavers", "Babson Beavers",
              "UMass Dartmouth Corsairs", "Suffolk Rams", "Castleton Spartans",
              "Castleton Spartans",
              "Southern Maine Huskies", "Coast Guard Bears", "RIC Anchormen", "RIC Anchormen",
              "UMass Dartmouth Corsairs", "Endicott Gulls", "Eastern Connecticut State Warriors",
              "Eastern Connecticut State Warriors", "Southern Maine Huskies",
              "Salve Regina Seahawks", "Plymouth State Panthers", "Plymouth State Panthers")

D3teamtotals$BABIP.x <- D3teamtotals$H.x / (D3teamtotals$AB - D3teamtotals$SO.x)
D3teamtotals <- D3teamtotals %>%
  mutate(BABIP.x = round(BABIP.x, 3))

D3teamtotals <- D3teamtotals %>%
  rename(
    X2B = `2B`,
    X3B = `3B`
  )
D3teamtotals$X1B <- D3teamtotals$H.x -
  D3teamtotals$X2B -
  D3teamtotals$X3B -
  D3teamtotals$HR.x

D3teamtotals <- D3teamtotals %>%
  mutate(
    wOBA.x = round(
      (
        0.69 * BB.x +
          0.72 * HBP.x +
          0.88 * X1B +
          1.25 * X2B +
          1.60 * X3B +
          2.00 * HR.x
      ) /
        (AB + BB.x + HBP.x + SF),
      3
    )
  )

D3teamtotals <- D3teamtotals %>%
  mutate(
    AB.y = BF - BB.y - HBP.y - IBB.y
  )


D3teamtotals <- D3teamtotals %>%
  mutate(
    BAA = round(H.y / AB.y, 3
    )
  )

D3teamtotals <- D3teamtotals %>%
  mutate(
    ISO = SLG - BA
  )

D3teamtotals <- D3teamtotals %>%
  mutate(
    SB_Attempts = SB + CS
  )

D3teamtotals <- D3teamtotals %>%
  mutate(
    SB_pct = round(SB / SB_Attempts, 3)
  )

D3teamtotals <- D3teamtotals %>%
  mutate(
    "HR/G" = HR.x / G.x
  )

D3teamtotals <- D3teamtotals %>%
  mutate(
    damage_profile = round(HR9 / H9, 3)
  )

D3teamtotals <- D3teamtotals %>%
  mutate(
    BABIP.y = round(H.y / (AB.y - SO.y), 3)
  )

D3teamtotals <- D3teamtotals %>%
  mutate(
    wOBA_plus = round(100 * wOBA.x / mean(D3teamtotals$wOBA.x), 3)
  )

D3teamtotals <- D3teamtotals %>%
  mutate(
    OPS_plus = round(100 * OPS / mean(D3teamtotals$OPS), 3)
  )

D3teamtotals <- D3teamtotals %>%
  mutate(
    ISO_plus = round(100 * ISO / mean(D3teamtotals$ISO), 3)
  )

D3teamtotals <- D3teamtotals %>%
  mutate(
    BAA_plus = round(100 * mean(D3teamtotals$BAA) / BAA, 3)
  )

D3teamtotals <- D3teamtotals %>%
  mutate(
    HR9_plus = round(100 * mean(D3teamtotals$HR9) / HR9, 3)
  )

D3teamtotals <- D3teamtotals %>%
  mutate(
    pitching_plus = round(0.6 * BAA_plus + 0.4 * HR9_plus, 3),
    team_plus = round(0.55 * wOBA_plus + 0.45 * pitching_plus, 3)
  )

##############################################################

#### report creation

opponent_offense <- D3teamtotals %>%
  select(
    Tm,
    wOBA_plus,
    OPS_plus,
    BA,
    HR.x,
    AB,
    BB.x,
    HBP.x,
    G.x
  )


offensive_report <- function(team_name, data) {
  
  team_row <- data %>%
    filter(Tm == team_name)
  
  team_row %>%
    mutate(
      HR_per_game = round(HR.x / G.x, 3),
      K_rate = round(SO.x / AB, 3),
      BBHBP_rate  = round((BB.x + HBP.x) / PA, 3),
      power_share = round(HR.x / AB, 3),
      
      offensive_style = case_when(
        wOBA_plus >= 115 & HR_per_game >= 1.0 ~ "Power-heavy offense",
        wOBA_plus >= 115 & BBHBP_rate >= 0.12 ~ "Patient, high OBP offense",
        wOBA_plus < 100  & HR_per_game >= 1.0 ~ "Boom-or-bust power",
        wOBA_plus < 100  & BA >= 0.280 ~ "Contact-dependent offense",
        TRUE ~ "Balanced offense"
      )
    ) %>%
    select(
      Tm,
      wOBA.x,
      wOBA_plus,
      OPS_plus,
      BA,
      HR_per_game,
      K_rate,
      BBHBP_rate,
      power_share,
      SB_Attempts,
      SB_pct,
      offensive_style
    )
}


schedule_2026 <- unique(schedule)

offensive_reports <- map_df(
  schedule_2026,
  offensive_report,
  data = D3teamtotals
)
View(offensive_reports)

#### pitching & defense reports

league_HR_rate <- mean(D3teamtotals$HR.y / D3teamtotals$AB.y, na.rm = TRUE)

pitching_report <- function(team_name, data) {
  
  team_row <- data %>%
    filter(Tm == team_name)
  
  team_row %>%
    mutate(
      HR_rate = round(HR.y / AB.y, 3),
      K_rate = round(SO.y / AB.y, 3),
      BBHBP_rate = round((BB.y + HBP.y) / BF, 3),
      HR_suppression = round(HR_rate / league_HR_rate, 3),
      
      pitching_style = case_when(
        HR_suppression < 0.85 & BBHBP_rate < 0.12 ~ "Power suppressor",
        HR_suppression > 1.15 & BBHBP_rate > 0.12 ~ "Volatile / HR-prone",
        BAA < 0.240 & BBHBP_rate < 0.08 ~ "Contact suppressor",
        TRUE ~ "Average / mixed profile"
      )
    ) %>%
    select(
      Tm,
      BAA,
      BAA_plus,
      ERA,
      RA9,
      HR_rate,
      K_rate,
      BBHBP_rate,
      HR9_plus,
      HR_suppression,
      pitching_style
    )
}

pitching_reports <- map_df(
  schedule_2026,
  pitching_report,
  data = D3teamtotals
)
View(pitching_reports)

#### matchup advantage scores

UMass_off <- offensive_report("UMass Boston Beacons", D3teamtotals)
UMass_pitch <- pitching_report("UMass Boston Beacons", D3teamtotals)

all_offense <- bind_rows(UMass_off, offensive_reports)
all_pitching <- bind_rows(UMass_pitch, pitching_reports)

matchup_score <- function(team_name, schedule, offense, pitching) {
  
  # League averages
  league_wOBA <- mean(offense$wOBA.x, na.rm = TRUE)
  league_HR   <- mean(pitching$HR_suppression, na.rm = TRUE)
  league_BB   <- mean(pitching$BBHBP_rate, na.rm = TRUE)
  
  results <- data.frame(
    opponent = schedule,
    matchup_score = NA,
    matchup_category = NA
  )
  
  for (i in seq_along(schedule)) {
    opp <- schedule[i]
    
    # Grab team and opponent data
    team_off   <- offense   %>% filter(Tm == team_name)  %>% slice(1)
    opp_off    <- offense   %>% filter(Tm == opp)        %>% slice(1)
    team_pitch <- pitching  %>% filter(Tm == team_name)  %>% slice(1)
    opp_pitch  <- pitching  %>% filter(Tm == opp)        %>% slice(1)
    
    # Skip if any data missing
    if(nrow(team_off) == 0 | nrow(opp_off) == 0 | nrow(team_pitch) == 0 | nrow(opp_pitch) == 0) next
    
    # Normalize stats relative to league
    team_off_wOBA_norm <- team_off$wOBA.x - league_wOBA
    opp_off_wOBA_norm  <- opp_off$wOBA.x  - league_wOBA
    team_pitch_HR_norm <- league_HR - team_pitch$HR_suppression   # fewer HR allowed → positive
    opp_pitch_HR_norm  <- opp_pitch$HR_suppression - league_HR   # opponent gives up more HR → positive
    team_pitch_BB_norm <- league_BB - team_pitch$BBHBP_rate      # fewer BB/HBP → positive
    opp_pitch_BB_norm  <- opp_pitch$BBHBP_rate - league_BB       # opponent gives up more BB/HBP → positive
    
    # Compute matchup score
    off_adv   <- team_off_wOBA_norm + (opp_pitch_HR_norm + opp_pitch_BB_norm)
    pitch_adv <- (team_pitch_HR_norm + team_pitch_BB_norm) - opp_off_wOBA_norm
    
    total_score <- round(off_adv + pitch_adv, 3)
    
    # Categorize
    category <- case_when(
      total_score > 0.02  ~ "Advantage",
      total_score < -0.02 ~ "Disadvantage",
      TRUE                ~ "Even"
    )
    
    results$matchup_score[i] <- total_score
    results$matchup_category[i] <- category
  }
  
  return(results)
}

matchups_2026 <- matchup_score(
  team_name = "UMass Boston Beacons",
  schedule = schedule_2026,
  offense = all_offense,
  pitching = all_pitching
)
View(matchups_2026)

#### upset risk index

library(dplyr)
library(stringr)

upset_risk_index <- function(team_name, schedule, offense, pitching, matchup_scores) {
  
  offense <- offense %>% mutate(Tm = str_to_lower(str_trim(Tm)))
  pitching <- pitching %>% mutate(Tm = str_to_lower(str_trim(Tm)))
  schedule <- str_to_lower(str_trim(schedule))
  team_name <- str_to_lower(str_trim(team_name))
  matchup_scores <- matchup_scores %>% 
    mutate(opponent = str_to_lower(str_trim(opponent)))
  
  results <- data.frame(
    opponent = schedule,
    matchup_score = NA,
    URI_raw = NA,
    URI = NA
  )
  
  league_wOBA.x <- mean(offense$wOBA.x, na.rm = TRUE)
  league_HR  <- mean(pitching$HR_suppression, na.rm = TRUE)
  league_BB  <- mean(pitching$BBHBP_rate, na.rm = TRUE)
  
  for(i in seq_along(schedule)) {
    opp <- schedule[i]
    
    team_off   <- offense %>% filter(Tm == team_name) %>% slice(1)
    team_pitch <- pitching %>% filter(Tm == team_name) %>% slice(1)
    opp_off    <- offense %>% filter(Tm == opp) %>% slice(1)
    opp_pitch  <- pitching %>% filter(Tm == opp) %>% slice(1)
    matchup    <- matchup_scores %>% filter(opponent == opp) %>% slice(1)
    
    if(nrow(team_off) == 0 | nrow(team_pitch) == 0 | nrow(opp_off) == 0 | nrow(opp_pitch) == 0 | nrow(matchup) == 0) {
      next
    }
    
    team_variance <- (team_off$HR_per_game + team_off$BBHBP_rate + team_pitch$HR_suppression + team_pitch$BBHBP_rate)
    
    opp_upside <- (opp_off$wOBA.x + opp_off$HR_per_game + opp_pitch$HR_suppression + opp_pitch$BBHBP_rate)
    
    base_adv <- matchup$matchup_score
    
    URI_raw <- team_variance * opp_upside * (1 - base_adv)
    
    results$matchup_score[i] <- round(base_adv, 3)
    results$URI_raw[i] <- round(URI_raw, 3)
  }
    results$URI <- (results$URI_raw - min(results$URI_raw, na.rm = TRUE)) /
    (max(results$URI_raw, na.rm = TRUE) - min(results$URI_raw, na.rm = TRUE))
  
  return(results)
}

uri_2026 <- upset_risk_index(
  team_name = "UMass Boston Beacons",
  schedule = schedule_2026,
  offense = all_offense,
  pitching = all_pitching,
  matchup_scores = matchups_2026
)
View(uri_2026)

#### run-style susceptibility model

run_style_susceptibility <- function(team_name, schedule, offense, pitching) {
  
  results <- data.frame(
    opponent = schedule,
    HR_susceptibility = NA,
    BB_susceptibility = NA,
    BA_susceptibility = NA,
    K_rate_susceptibility = NA,
    run_style = NA
  )
  
  league_HR  <- mean(pitching$HR_suppression, na.rm = TRUE)
  league_BB  <- mean(pitching$BBHBP_rate, na.rm = TRUE)
  league_BA  <- mean(pitching$BAA, na.rm = TRUE)
  league_K_rate <- mean(offense$K_rate, na.rm = TRUE)
  
  for (i in seq_along(schedule)) {
    opp <- schedule[i]
    
    opp_pitch <- pitching %>% filter(Tm == opp) %>% slice(1)
    
    if(nrow(opp_pitch) == 0) next
    
    HR_sus <- opp_pitch$HR_suppression - league_HR
    BB_sus <- opp_pitch$BBHBP_rate - league_BB
    BA_sus <- opp_pitch$BAA - league_BA
    K_rate_sus <- opp_pitch$K_rate - league_K_rate
    
    run_style <- case_when(
      HR_sus > 0.05 ~ "Attack with power hitters",
      BB_sus > 0.05 ~ "Patient, take walks",
      BA_sus > 0.05 ~ "Contact hitters can succeed",
      K_rate_sus > 0.05 ~ "Strikeout Reliant",
      TRUE          ~ "Balanced attack"
    )
    
    results$HR_susceptibility[i] <- round(HR_sus,3)
    results$BB_susceptibility[i] <- round(BB_sus,3)
    results$BA_susceptibility[i] <- round(BA_sus,3)
    results$K_rate_susceptibility[i] <- round(K_rate_sus,3)
    results$run_style[i] <- run_style
  }
  
  return(results)
}

rss_2026 <- run_style_susceptibility(
  team_name = "UMass Boston Beacons",
  schedule = schedule_2026,
  offense = all_offense,
  pitching = all_pitching
)

View(rss_2026)
