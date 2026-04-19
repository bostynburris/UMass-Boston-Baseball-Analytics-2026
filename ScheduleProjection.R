
library(tidyverse)

littleeastconference <- c("UMass Boston", "Eastern Connecticut State", "Western Connecticut State", "UMass Dartmouth",
                          "Southern Maine", "Keene State", "Plymouth State", "Castleton State", "Rhode Island")

conferenceopponents <- c("Eastern Connecticut State", "Western Connecticut State", "UMass Dartmouth",
                         "Southern Maine", "Keene State", "Plymouth State", "Castleton State", "Rhode Island")

outofconference <- c("Bridgewater State", "Worcester Polytech", "MIT", "Lasell", "SUNY Brockport", "Saint Joes",
                     "Western New England", "Ohio Northern", "Rutgers-Camden", "Rowan", "Mount St Vincent",
                     "Tufts", "J&W", "Nichols", "Roger Williams", "Babson", "Suffolk", "Coast Guard", "Endicott",
                     "Salve Regina")

Doubleheaders <- c("Eastern Connecticut State", "Western Connecticut State", "UMass Dartmouth",
                   "Southern Maine", "Keene State", "Plymouth State", "Castleton State", "Rhode Island", "SUNY Brockport",
                   "Babson")

schedule <- c("Bridgewater State", "Worcester Polytech", "MIT", "Lasell", "SUNY Brockport", "SUNY Brockport",
              "Saint Joes", "Western New England", "Ohio Northern", "Rutgers-Camden", "Rowan", "Mount St Vincent",
              "Western Connecticut State", "Western Connecticut State", "Tufts", "J&W", "Keene State", "Keene State",
              "Nichols", "Roger Williams", "Babson", "Babson", "UMass Dartmouth", "Suffolk", "Castleton State", "Castleton State",
              "Southern Maine", "Coast Guard", "Rhode Island", "Rhode Island", "UMass Dartmouth", "Eastern Connecticut State",
              "Eastern Connecticut State", "Southern Maine", "Salve Regina", "Plymouth State", "Plymouth State")

win_prob <- c(0.7, 0.75, 0.7, 0.8, 0.6, 0.45, 0.45, 0.7, 0.7, 0.5, 0.5, 0.35, 0.6, 0.95, 0.95, 0.6, 0.55, 0.7, 0.7, 0.55,
              0.6, 0.6, 0.5, 0.5, 0.65, 0.9, 0.9, 0.9, 0.65, 0.55, 0.65, 0.65, 0.65, 0.30, 0.4, 0.4, 0.65, 0.30, 0.85, 0.85)

simulate_season <- function(win_prob) {
  rbinom(length(win_prob), size = 1, prob = win_prob)
}
season_results <- simulate_season(win_prob)
sum(season_results)


simulate_many_seasons <- function(win_probs, n_sims = 20000) {
  wins <- numeric(n_sims)
  
  for (s in 1:n_sims) {
    wins[s] <- sum(simulate_season(win_prob))
  }
  
  return(wins)
}

season_wins <- simulate_many_seasons(win_prob, n_sims = 20000)

mean(season_wins)
sd(season_wins)

table(season_wins) / length(season_wins)

conference_games <- data.frame(
  game_id = 1:72,
  away = c("KSC", "KSC", "USM", "USM", "WCSU", "WCSU", "VTSUC", "VTSUC", "ECSU", "PSU", "UMB", "UMB", "UMD", "UMD", "RIC", "RIC",
             "RIC", "RIC", "USM", "USM", "ECSU", "ECSU", "KSC", "KSC", "RIC", "UMB", "KSC", "PSU", "PSU", "PSU", "VTSUC",
             "VTSUC", "RIC", "RIC", "USM", "USM", "UMB", "PSU", "RIC", "ECSU", "VTSUC", "VTSUC", "KSC", "KSC", "UMB", "UMB",
             "UMD", "UMD", "USM", "VTSUC", "UMD", "RIC", "PSU", "PSU", "UMB", "UMB", "KSC", "KSC", "VTSUC", "VTSUC",
             "VTSUC", "USM", "UMD", "WCSU", "UMD", "UMD", "WCSU", "WCSU", "ECSU", "ECSU", "PSU", "PSU"),
  
  home = c("RIC", "RIC", "UMD", "UMD", "UMB", "UMB", "ECSU", "ECSU", "PSU", "ECSU", "KSC", "KSC", "WCSU", "WCSU", "USM", "USM",
             "VTSUC", "VTSUC", "WCSU", "WCSU", "UMD", "UMD", "PSU", "PSU", "WCSU", "UMD", "VTSUC", "USM", "WCSU", "WCSU", "UMB",
             "UMB", "ECSU", "ECSU", "KSC", "KSC", "USM", "VTSUC", "UMD", "WCSU", "WCSU", "WCSU", "ECSU", "ECSU", "RIC", "RIC",
             "PSU", "PSU", "PSU", "KSC", "UMB", "WCSU", "RIC", "RIC", "ECSU", "ECSU", "UMD", "UMD", "USM", "USM",
             "PSU", "UMB", "RIC", "ECSU", "VTSUC", "VTSUC", "KSC", "KSC", "USM", "USM", "UMB", "UMB"),
  
  away_win = c(0.5, 0.5, 0.6, 0.6, 0.05, 0.05, 0.15, 0.15, 0.85, 0.15, 0.7, 0.7, 0.9, 0.9, 0.5, 0.5, 0.65, 0.65, 0.9, 0.9, 0.8, 0.8, 0.7, 0.7,
               0.8, 0.65, 0.8, 0.25, 0.8, 0.8, 0.1, 0.1, 0.15, 0.15, 0.4, 0.4, 0.65, 0.5, 0.5, 0.95, 0.75, 0.75, 0.2, 0.2, 0.7, 0.7,
               0.6, 0.6, 0.75, 0.15, 0.35, 0.85, 0.25, 0.25, 0.45, 0.45, 0.6, 0.6, 0.2, 0.2, 0.5, 0.35, 0.5, 0.05, 0.65, 0.65, 0.1, 0.1,
               0.8, 0.8, 0.1, 0.1)
)

teams <- sort(unique(c(conference_games$away, conference_games$home)))
teams

simulate_conference_games <- function(games) {
  
  results <- rbinom(nrow(games), 1, games$away_win)
  
  games$winner <- ifelse(results == 1, games$away, games$home)
  games$loser  <- ifelse(results == 1, games$home, games$away)
  
  return(games)
}

build_standings <- function(sim_games, teams) {
  
  wins <- table(sim_games$winner)
  losses <- table(sim_games$loser)
  
  standings <- data.frame(
    team = teams,
    wins = as.numeric(wins[teams]),
    losses = as.numeric(losses[teams])
  )
  
  standings$wins[is.na(standings$wins)] <- 0
  standings$losses[is.na(standings$losses)] <- 0
  
  return(standings)
}

head_to_head <- function(sim_games, team1, team2) {
  
  h2h_games <- sim_games[
    (sim_games$away == team1 & sim_games$home == team2) |
      (sim_games$away == team2 & sim_games$home == team1),
  ]
  
  sum(h2h_games$winner == team1)
}

break_ties <- function(standings, sim_games) {
  
  standings <- standings[order(-standings$wins), ]
  
  i <- 1
  while (i < nrow(standings)) {
    
    tied <- which(standings$wins == standings$wins[i])
    
    if (length(tied) > 1) {
      tied_teams <- standings$team[tied]
      
      h2h_wins <- sapply(tied_teams, function(t) {
        sum(sapply(tied_teams, function(o) {
          if (t != o) head_to_head(sim_games, t, o) else 0
        }))
      })
      
      standings[tied, ] <- standings[tied[order(-h2h_wins)], ]
    }
    
    i <- max(tied) + 1
  }
  
  standings$seed <- 1:nrow(standings)
  return(standings)
}

simulate_conference <- function(games, teams) {
  
  sim_games <- simulate_conference_games(games)
  
  standings <- build_standings(sim_games, teams)
  
  standings <- break_ties(standings, sim_games)
  
  standings
}

n_sims <- 20000

seed_results <- matrix(0, nrow = length(teams), ncol = 9,
                       dimnames = list(teams, paste0("Seed_", 1:9)))

for (i in 1:n_sims) {
  final_standings <- simulate_conference(conference_games, teams)
  
  for (t in teams) {
    s <- final_standings$seed[final_standings$team == t]
    seed_results[t, s] <- seed_results[t, s] + 1
  }
}

seed_probs <- seed_results / n_sims

top6_prob <- rowSums(seed_probs[, 1:6])

View(seed_probs)
View(top6_prob)

#############################################################################

#### win leverage model to identify games with biggest impact

target_team <- "UMB"

umb_games <- conference_games[
  conference_games$home == target_team |
    conference_games$away == target_team,
]

nrow(umb_games)

simulate_conference_games_forced <- function(games) {
  
  results <- rbinom(nrow(games), 1, games$away_win)
  
  games$winner <- ifelse(results == 1, games$away, games$home)
  games$loser  <- ifelse(results == 1, games$home, games$away)
  
  forced_idx <- which(games$forced)
  
  if (length(forced_idx) > 0) {
    games$winner[forced_idx] <- games$forced_winner[forced_idx]
    games$loser[forced_idx] <- ifelse(
      games$home[forced_idx] == games$forced_winner[forced_idx],
      games$away[forced_idx],
      games$home[forced_idx]
    )
  }
  
  games
}


run_conditioned_sims <- function(games, n_sims = 3000) {
  
  seeds <- integer(n_sims)
  
  for (i in 1:n_sims) {
    final_standings <- simulate_conference(games, teams)
    seeds[i] <- final_standings$seed[final_standings$team == target_team]
  }
  
  list(
    top6 = mean(seeds <= 6),
    avg_seed = mean(seeds)
  )
}

leverage_for_game <- function(game_row, n_sims = 3000) {
  
  game_id <- game_row$game_id
  opponent <- ifelse(
    game_row$home == target_team,
    game_row$away,
    game_row$home
  )
  
  games_win <- conference_games
  games_win$forced <- FALSE
  games_win$forced_winner <- NA
  games_win$forced[games_win$game_id == game_id] <- TRUE
  games_win$forced_winner[games_win$game_id == game_id] <- target_team
  
  win_outcome <- run_conditioned_sims(games_win, n_sims)
  
  games_loss <- conference_games
  games_loss$forced <- FALSE
  games_loss$forced_winner <- NA
  games_loss$forced[games_loss$game_id == game_id] <- TRUE
  games_loss$forced_winner[games_loss$game_id == game_id] <- opponent
  
  loss_outcome <- run_conditioned_sims(games_loss, n_sims)
  
  data.frame(
    game_id = game_id,
    opponent = opponent,
    leverage_top6 = win_outcome$top6 - loss_outcome$top6,
    leverage_seed = loss_outcome$avg_seed - win_outcome$avg_seed
  )
}

umb_games <- conference_games[
  conference_games$home == target_team |
    conference_games$away == target_team,
]


system.time(
  leverage_for_game(umb_games[1, ], n_sims = 3000)
)

leverage_results <- do.call(
  rbind,
  lapply(seq_len(nrow(umb_games)), function(i) {
    leverage_for_game(umb_games[i, ])
  })
)

leverage_results[order(-leverage_results$leverage_top6), ]
View(leverage_results)


#######################################################################

schedule_mar10 <- c("Mitchell", "Bridgewater State", "Worcester Polytech", "MIT", "SUNY Brockport", "SUNY Brockport",
              "Saint Joes", "Western New England", "Stockton", "Ohio Northern", "Rutgers-Camden", "Rowan", "Mount St Vincent",
              "Western Connecticut State", "Western Connecticut State", "Tufts", "J&W", "Keene State", "Keene State",
              "Nichols", "Roger Williams", "Babson", "Babson", "UMass Dartmouth", "Suffolk", "Castleton State", "Castleton State",
              "Southern Maine", "Coast Guard", "Rhode Island", "Rhode Island", "UMass Dartmouth", "Eastern Connecticut State",
              "Eastern Connecticut State", "Southern Maine", "Salve Regina", "Plymouth State", "Plymouth State")

win_prob_mar10 <- c(0, 0.7, 0.75, 1, 0, 1, 0.45, 0.7, 0.6, 0.5, 0.55, 0.5, 0.35, 0.6, 0.95, 0.95, 0.6, 0.55, 0.7, 0.7, 0.55,
              0.6, 0.6, 0.5, 0.5, 0.65, 0.9, 0.9, 0.9, 0.65, 0.55, 0.65, 0.65, 0.65, 0.30, 0.4, 0.4, 0.65, 0.30, 0.85, 0.85)

simulate_season <- function(win_prob_mar10) {
  rbinom(length(win_prob_mar10), size = 1, prob = win_prob_mar10)
}
season_results <- simulate_season(win_prob_mar10)
sum(season_results)

season_wins_mar10 <- simulate_many_seasons(win_prob_mar10, n_sims = 20000)

mean(season_wins_mar10)
sd(season_wins_mar10)

table(season_wins_mar10) / length(season_wins_mar10)

#######################################################################


schedule_mar19 <- c("Mitchell", "Bridgewater State", "Worcester Polytech", "MIT", "SUNY Brockport", "SUNY Brockport",
                    "Saint Joes", "Western New England", "Stockton", "Ohio Northern", "Bethel", "Rowan", "Mount St Vincent",
                    "Western Connecticut State", "Western Connecticut State", "Tufts", "J&W", "Keene State", "Keene State",
                    "Nichols", "Roger Williams", "Babson", "Babson", "UMass Dartmouth", "Suffolk", "Castleton State", "Castleton State",
                    "Southern Maine", "Coast Guard", "Rhode Island", "Rhode Island", "UMass Dartmouth", "Eastern Connecticut State",
                    "Eastern Connecticut State", "Southern Maine", "Salve Regina", "Plymouth State", "Plymouth State")

win_prob_mar19 <- c(0, 0.7, 0.75, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0.95, 0.95, 0.6, 0.55, 0.7, 0.7, 0.55,
                    0.6, 0.6, 0.5, 0.5, 0.65, 0.9, 0.9, 0.9, 0.65, 0.55, 0.65, 0.65, 0.65, 0.30, 0.4, 0.4, 0.65, 0.30, 0.85, 0.85)

simulate_season <- function(win_prob_mar19) {
  rbinom(length(win_prob_mar19), size = 1, prob = win_prob_mar10)
}
season_results <- simulate_season(win_prob_mar19)
sum(season_results)

season_wins_mar19 <- simulate_many_seasons(win_prob_mar19, n_sims = 20000)

mean(season_wins_mar19)
sd(season_wins_mar19)

table(season_wins_mar19) / length(season_wins_mar19)

#######################################################################

conference_games_Apr8 <- data.frame(
  game_id = 1:72,
  away = c("KSC", "KSC", "USM", "USM", "WCSU", "WCSU", "VTSUC", "VTSUC", "ECSU", "PSU", "UMB", "UMB", "UMD", "UMD", "RIC", "RIC",
           "RIC", "RIC", "USM", "USM", "ECSU", "ECSU", "KSC", "KSC", "RIC", "UMB", "KSC", "PSU", "PSU", "PSU", "VTSUC",
           "VTSUC", "RIC", "RIC", "USM", "USM", "UMB", "PSU", "RIC", "ECSU", "VTSUC", "VTSUC", "KSC", "KSC", "UMB", "UMB",
           "UMD", "UMD", "USM", "VTSUC", "UMD", "RIC", "PSU", "PSU", "UMB", "UMB", "KSC", "KSC", "VTSUC", "VTSUC",
           "VTSUC", "USM", "UMD", "WCSU", "UMD", "UMD", "WCSU", "WCSU", "ECSU", "ECSU", "PSU", "PSU"),
  
  home = c("RIC", "RIC", "UMD", "UMD", "UMB", "UMB", "ECSU", "ECSU", "PSU", "ECSU", "KSC", "KSC", "WCSU", "WCSU", "USM", "USM",
           "VTSUC", "VTSUC", "WCSU", "WCSU", "UMD", "UMD", "PSU", "PSU", "WCSU", "UMD", "VTSUC", "USM", "WCSU", "WCSU", "UMB",
           "UMB", "ECSU", "ECSU", "KSC", "KSC", "USM", "VTSUC", "UMD", "WCSU", "WCSU", "WCSU", "ECSU", "ECSU", "RIC", "RIC",
           "PSU", "PSU", "PSU", "KSC", "UMB", "WCSU", "RIC", "RIC", "ECSU", "ECSU", "UMD", "UMD", "USM", "USM",
           "PSU", "UMB", "RIC", "ECSU", "VTSUC", "VTSUC", "KSC", "KSC", "USM", "USM", "UMB", "UMB"),
  
  away_win = c(1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0.7, 0.7,
               1, 0, 1, 0, 0.15, 0.65, 0.65, 0.2, 0.2, 0.1, 0.1, 0.6, 0.6, 0.4, 0.4, 0.25, 0.95, 0.8, 0.8, 0.35, 0.35, 0.85,
               0.85, 0.75, 0.75, 0.9, 0.3, 0.4, 0, 0.5, 0.5, 0.45, 0.45, 0.5, 0.5, 0.2, 0.2, 0.8, 0.55, 0.8, 0.7, 0.7, 0.05, 0.05,
               0.6, 0.6, 0.1, 0.1)
)

teams_Apr8 <- sort(unique(c(conference_games_Apr8$away, conference_games_Apr8$home)))
teams_Apr8

seed_results_Apr8 <- matrix(0, nrow = length(teams_Apr8), ncol = 9,
                       dimnames = list(teams_Apr8, paste0("Seed_", 1:9)))

for (i in 1:n_sims) {
  final_standings <- simulate_conference(conference_games_Apr8, teams_Apr8)
  
  for (t in teams) {
    s <- final_standings$seed[final_standings$team == t]
    seed_results_Apr8[t, s] <- seed_results_Apr8[t, s] + 1
  }
}

seed_probs_Apr8 <- seed_results_Apr8 / n_sims

top6_prob_Apr8 <- rowSums(seed_probs_Apr8[, 1:6])

View(seed_probs_Apr8)
View(top6_prob_Apr8)

#######################################################################

schedule_Apr8 <- c("Mitchell", "Bridgewater State", "Worcester Polytech", "MIT", "SUNY Brockport", "SUNY Brockport",
                    "Saint Joes", "Western New England", "Stockton", "Ohio Northern", "Bethel", "Rowan", "Mount St Vincent",
                    "Western Connecticut State", "Western Connecticut State", "Tufts", "J&W", "Keene State", "Keene State",
                    "Nichols", "Roger Williams", "Babson", "Babson", "UMass Dartmouth", "Suffolk", "Castleton State", "Castleton State",
                    "Southern Maine", "Coast Guard", "Rhode Island", "Rhode Island", "UMass Dartmouth", "Eastern Connecticut State",
                    "Eastern Connecticut State", "Endicott College", "Southern Maine", "Salve Regina", "Plymouth State", "Plymouth State")

current_wins <- 13
current_losses <- 10
games_played <- current_wins + current_losses
remaining_probs <- win_prob_Apr8[(games_played + 1):length(win_prob_Apr8)]

win_prob_Apr8 <- c(0, 0.7, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0.55, 1, 0, 1,
                    1, 1, 0, 0, 0.8, 0.85, 0.85, 0.45, 0.6, 0.8, 0.8, 0.65, 0.65, 0.45, 0.45, 0.25, 0.45, 0.2, 0.9, 0.9)

simulate_season <- function(remaining_probs, current_wins) {
  future_wins <- sum(rbinom(length(remaining_probs), size = 1, prob = remaining_probs))
  total_wins <- current_wins + future_wins
  return(total_wins)
}

simulate_many_seasons <- function(remaining_probs, current_wins, n_sims = 20000) {
  replicate(n_sims, simulate_season(remaining_probs, current_wins))
}

season_wins_Apr8 <- simulate_many_seasons(remaining_probs, current_wins, 20000)
season_results <- simulate_season(win_prob_Apr8, current_wins)
sum(season_results)

season_wins_Apr8 <- simulate_many_seasons(win_prob_Apr8, current_wins, n_sims = 20000)

mean(season_wins_Apr8)
sd(season_wins_Apr8)

table(season_wins_Apr8) / length(season_wins_Apr8)

#######################################################################


conference_games_Apr18 <- data.frame(
  game_id = 1:72,
  away = c("KSC", "KSC", "USM", "USM", "WCSU", "WCSU", "VTSUC", "VTSUC", "ECSU", "PSU", "UMB", "UMB", "UMD", "UMD", "RIC", "RIC",
           "RIC", "RIC", "USM", "USM", "ECSU", "ECSU", "KSC", "KSC", "RIC", "UMB", "KSC", "PSU", "PSU", "PSU", "VTSUC",
           "VTSUC", "RIC", "RIC", "USM", "USM", "UMB", "PSU", "RIC", "ECSU", "VTSUC", "VTSUC", "KSC", "KSC", "UMB", "UMB",
           "UMD", "UMD", "USM", "VTSUC", "UMD", "RIC", "PSU", "PSU", "UMB", "UMB", "KSC", "KSC", "VTSUC", "VTSUC",
           "VTSUC", "USM", "UMD", "WCSU", "UMD", "UMD", "WCSU", "WCSU", "ECSU", "ECSU", "PSU", "PSU"),
  
  home = c("RIC", "RIC", "UMD", "UMD", "UMB", "UMB", "ECSU", "ECSU", "PSU", "ECSU", "KSC", "KSC", "WCSU", "WCSU", "USM", "USM",
           "VTSUC", "VTSUC", "WCSU", "WCSU", "UMD", "UMD", "PSU", "PSU", "WCSU", "UMD", "VTSUC", "USM", "WCSU", "WCSU", "UMB",
           "UMB", "ECSU", "ECSU", "KSC", "KSC", "USM", "VTSUC", "UMD", "WCSU", "WCSU", "WCSU", "ECSU", "ECSU", "RIC", "RIC",
           "PSU", "PSU", "PSU", "KSC", "UMB", "WCSU", "RIC", "RIC", "ECSU", "ECSU", "UMD", "UMD", "USM", "USM",
           "PSU", "UMB", "RIC", "ECSU", "VTSUC", "VTSUC", "KSC", "KSC", "USM", "USM", "UMB", "UMB"),
  
  away_win = c(1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1,
               1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0,
               0, 1, 1, 0.9, 0.2, 0.4, 0, 0.5, 0.5, 0.45, 0.45, 0.5, 0.5, 0.2, 0.2, 0.8, 0.55, 0.8, 0.7, 0.7, 0.7, 0.05, 0.05,
               0.6, 0.6, 0.1, 0.1)
)

teams_Apr18 <- sort(unique(c(conference_games_Apr18$away, conference_games_Apr18$home)))
teams_Apr18

seed_results_Apr18 <- matrix(0, nrow = length(teams_Apr18), ncol = 9,
                            dimnames = list(teams_Apr18, paste0("Seed_", 1:9)))

for (i in 1:n_sims) {
  final_standings <- simulate_conference(conference_games_Apr18, teams_Apr18)
  
  for (t in teams) {
    s <- final_standings$seed[final_standings$team == t]
    seed_results_Apr18[t, s] <- seed_results_Apr18[t, s] + 1
  }
}

seed_probs_Apr18 <- seed_results_Apr18 / n_sims

top6_prob_Apr18 <- rowSums(seed_probs_Apr18[, 1:6])

View(seed_probs_Apr18)
View(top6_prob_Apr18)

#######################################################################

schedule_Apr18 <- c("Mitchell", "Curry", "Worcester Polytech", "MIT", "SUNY Brockport", "SUNY Brockport",
                   "Saint Joes", "Western New England", "Stockton", "Ohio Northern", "Bethel", "Rowan", "Mount St Vincent",
                   "Western Connecticut State", "Western Connecticut State", "Tufts", "J&W", "Keene State", "Keene State",
                   "Nichols", "Roger Williams", "Babson", "Babson", "UMass Dartmouth", "Suffolk", "Castleton State", "Castleton State",
                   "Southern Maine", "Coast Guard", "Rhode Island", "Rhode Island", "UMass Dartmouth", "Eastern Connecticut State",
                   "Eastern Connecticut State", "Endicott College", "Southern Maine", "Salve Regina", "Plymouth State", "Plymouth State")

current_wins <- 16
current_losses <- 14
games_played <- current_wins + current_losses

win_prob_Apr18 <- c(0, 0.85, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0.55, 1, 0, 1,
                   1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0.65, 0.65, 0.45, 0.45, 0.25, 0.45, 0.2, 0.9, 0.9)

remaining_probs <- win_prob_Apr18[(games_played + 1):length(win_prob_Apr18)]

simulate_season <- function(remaining_probs, current_wins) {
  future_wins <- sum(rbinom(length(remaining_probs), size = 1, prob = remaining_probs))
  total_wins <- current_wins + future_wins
  return(total_wins)
}

simulate_many_seasons <- function(remaining_probs, current_wins, n_sims = 20000) {
  replicate(n_sims, simulate_season(remaining_probs, current_wins))
}

season_wins_Apr18 <- simulate_many_seasons(remaining_probs, current_wins, 20000)
season_results <- simulate_season(remaining_probs, current_wins)
sum(season_results)

season_wins_Apr18 <- simulate_many_seasons(remaining_probs, current_wins, n_sims = 20000)

mean(season_wins_Apr18)
sd(season_wins_Apr18)

table(season_wins_Apr18) / length(season_wins_Apr18)
