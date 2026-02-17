library(baseballr)
library(tidyverse)

View(preseasonhitterproj)

preseasonhitterproj <- preseasonhitterproj %>%
  mutate(
    proj_iso = proj_slg - proj_avg
  )

event_probs <- function(row) {
  
  avg <- row$proj_avg
  iso <- row$proj_iso
  k_rate <- row$proj_k_pct
  bb_rate <- row$proj_bb_pct
  hbp_rate <- row$proj_hbp_pct
  bip_rate <- row$proj_bip_pct
  
  ab_rate <- 1 - bb_rate - hbp_rate
  
  h_rate <- avg * ab_rate
  
  iso_pa <- iso * ab_rate
  
  hr_share <- min(0.40, 0.08 + 3.0 * iso)
  
  triple_share <- 0.04
  
  double_share <- 1 - hr_share - triple_share
  
  hr_rate <- (hr_share * iso_pa) / 3
  triple_rate <- (triple_share * iso_pa) / 2
  double_rate <- double_share * iso_pa
  
  single_rate <- h_rate - double_rate - triple_rate - hr_rate
  
  bip_out_rate <- bip_rate - h_rate
  
  probs <- c(
    single = single_rate,
    double = double_rate,
    triple = triple_rate,
    hr = hr_rate,
    walk = bb_rate,
    hbp = hbp_rate,
    out = k_rate + bip_out_rate
  )
  
  return(probs / sum(probs))
}

state <- list(
  outs = 0,
  first = 0,
  second = 0,
  third = 0,
  runs = 0
)


simulate_pa <- function(state, probs) {
  
  event <- sample(names(probs), size = 1, prob = probs)
  
  if (event == "out") {
    state$outs <- state$outs + 1
  }
  
  if (event == "walk") {
    if (state$first == 1 & state$second == 1 & state$third == 1) {
      state$runs <- state$runs + 1
    }
    state$third <- state$second
    state$second <- state$first
    state$first <- 1
  }
  
  if (event == "single") {
    state$runs <- state$runs + state$third + state$second
    state$third <- state$first
    state$second <- 0
    state$first <- 1
  }
  
  if (event == "double") {
    state$runs <- state$runs + state$third + state$second + state$first
    state$third <- 0
    state$second <- 1
    state$first <- 0
  }
  
  if (event == "triple") {
    state$runs <- state$runs + state$third + state$second + state$first
    state$third <- 1
    state$second <- 0
    state$first <- 0
  }
  
  if (event == "hr") {
    state$runs <- state$runs + state$third + state$second + state$first + 1
    state$third <- 0
    state$second <- 0
    state$first <- 0
  }
  
  return(state)
}

simulate_inning <- function(lineup_probs, start_index) {
  
  state <- list(outs = 0, first = 0, second = 0, third = 0, runs = 0)
  batter_index <- start_index
  
  while (state$outs < 3) {
    
    probs <- lineup_probs[[batter_index]]
    state <- simulate_pa(state, probs)
    
    batter_index <- ifelse(batter_index == 9, 1, batter_index + 1)
  }
  
  return(list(runs = state$runs, next_index = batter_index))
}

simulate_game <- function(lineup_probs) {
  
  total_runs <- 0
  batter_index <- 1
  
  for (inning in 1:9) {
    result <- simulate_inning(lineup_probs, batter_index)
    total_runs <- total_runs + result$runs
    batter_index <- result$next_index
  }
  
  return(total_runs)
}

test_probs <- event_probs(preseasonhitterproj[1,])
sum(test_probs)
test_probs

#### test of probabilities on Andrew Carlson

ab_rate <- 1 - test_probs["walk"] - test_probs["hbp"]
hit_rate <- test_probs["single"] + test_probs["double"] +
  test_probs["triple"] + test_probs["hr"]

reconstructed_avg <- hit_rate / ab_rate
reconstructed_avg

###################################################################

#### lineup optimization simulations

lineup_probs <- lapply(1:nrow(preseasonhitterproj), function(i) {
  event_probs(preseasonhitterproj[i, ])
})

simulate_games <- function(lineup_probs, n = 500) {
  runs <- replicate(n, simulate_game(lineup_probs))
  return(list(
    mean = mean(runs),
    variance = var(runs)
  ))
}

evaluate_lineup <- function(lineup_probs, lambda = 0.15, n = 500) {
  sim <- simulate_games(lineup_probs, n)
  stability_score <- sim$mean - lambda * sim$variance
  return(c(
    mean = sim$mean,
    variance = sim$variance,
    score = stability_score
  ))
}

openingdayhitters <- c(
  'Justin Gouveia', 'Brandon Gaer', 'Breon Parker', 'Elliot Miles',
  'Max Garner', 'Chris Bissaillon', 'Nick Farnacci', 'N. Piscionere',
  'Jayden LaFleur', 'Ryan Slack', 'Caden Lindskog', 'P. Larkins',
  'Dylan Agvent', 'Ben Sherry', 'Wes Sturrup'
)

openingdaycandidates <- match(openingdayhitters, preseasonhitterproj$player)
candidate_df <- preseasonhitterproj[openingdaycandidates, ]
candidate_probs <- lineup_probs[openingdaycandidates]

combos <- t(combn(15, 9))
dim(combos)

results <- lapply(1:nrow(combos), function(i) {
  combo_indices <- combos[i, ]
  combo_df <- candidate_df[combo_indices, ]
  combo_probs <- candidate_probs[combo_indices]
  
  eval <- evaluate_lineup(combo_probs, lambda = 0.18, n = 500)
  
  list(
    players = combo_df$player,
    mean = eval["mean"],
    variance = eval["variance"],
    score = eval["score"]
  )
})

results_lineup <- do.call(rbind, lapply(results, function(x) {
  data.frame(
    lineup = paste(x$players, collapse = " | "),
    mean = as.numeric(x$mean),
    variance = as.numeric(x$variance),
    score = as.numeric(x$score),
    stringsAsFactors = FALSE
  )
}))

top_lineups <- results_lineup[order(-results_lineup$score), ][1:50, ]
top_combos_indices <- sapply(top_lineups$lineup, function(lu) {
  match(unlist(strsplit(lu, " \\| ")), candidate_df$player)
})

optimize_order <- function(combo_df, lineup_probs, lambda = 0.18, n_sim = 500) {
  order_idx <- order(-combo_df$proj_obp)
  current_probs <- lineup_probs[order_idx]
  best_eval <- evaluate_lineup(current_probs, lambda, n = n_sim)
  improved <- TRUE
  iter_count <- 0
  
  while(improved & iter_count < 3) {
    iter_count <- iter_count + 1
    improved <- FALSE
    for(i in 1:8){
      for(j in (i+1):9){
        new_order <- order_idx
        temp <- new_order[i]
        new_order[i] <- new_order[j]
        new_order[j] <- temp
        
        new_probs <- lineup_probs[new_order]
        new_eval <- evaluate_lineup(new_probs, lambda, n = n_sim)
        
        if(new_eval["score"] > best_eval["score"]){
          order_idx <- new_order
          best_eval <- new_eval
          improved <- TRUE
        }
      }
    }
  }
  return(list(order = order_idx, evaluation = best_eval))
}

optimized_results <- lapply(1:nrow(top_lineups), function(i) {
  combo_indices <- top_combos_indices[, i]
  combo_df <- candidate_df[combo_indices, ]
  combo_probs <- candidate_probs[combo_indices]
  
  optimized <- optimize_order(combo_df, combo_probs, lambda = 0.18, n_sim = 500)
  
  data.frame(
    lineup = paste(combo_df$player[optimized$order], collapse = " | "),
    mean = optimized$evaluation["mean"],
    variance = optimized$evaluation["variance"],
    score = optimized$evaluation["score"],
    stringsAsFactors = FALSE
  )
})

final_lineups <- do.call(rbind, optimized_results)
final_lineups <- final_lineups[order(-final_lineups$score), ]
View(final_lineups)
