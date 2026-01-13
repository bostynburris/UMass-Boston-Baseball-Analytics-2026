
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

