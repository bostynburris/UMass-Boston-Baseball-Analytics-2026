install.packages("baseballr")
library(baseballr)
library(tidyverse)
library(readr)

##################################################################

fastballrapsodo <- read_csv("fastballrapsodo.csv")
fastballrapsodo <- fastballrapsodo %>%
  mutate(
    pitch_type = 'fastball'
  )

fastballrapsodo2 <- read_csv("fastballrapsodo2.csv")
fastballrapsodo2 <- fastballrapsodo2 %>%
  mutate(
    pitch_type = 'fastball'
  )

fastballrapsodo3 <- read_csv("fastballrapsodo3.csv")
fastballrapsodo3 <- fastballrapsodo3 %>%
  mutate(
    pitch_type = 'fastball'
  )

cutterrapsodo <- read_csv("cutterrapsodo.csv")
cutterrapsodo <- cutterrapsodo %>%
  mutate(
    pitch_type = 'cutter'
  )

curveballrapsodo <- read_csv("curveballrapsodo.csv")
curveballrapsodo <- curveballrapsodo %>%
  mutate(
    pitch_type = 'curveball'
  )
curveballrapsodo

sliderrapsodo <- read_csv("sliderrapsodo.csv")
sliderrapsodo <- sliderrapsodo %>%
  mutate(
    pitch_type = 'slider'
  )

sliderrapsodo2 <- read_csv("sliderrapsodo2.csv")
sliderrapsodo2 <- sliderrapsodo2 %>%
  mutate(
    pitch_type = 'slider'
  )

twoseamrapsodo <- read_csv("twoseamrapsodo.csv")
twoseamrapsodo <- twoseamrapsodo %>%
  mutate(
    pitch_type = 'twoseamfastball'
  )

changeuprapsodo <- read_csv("changeuprapsodo.csv")
changeuprapsodo <- changeuprapsodo %>%
  mutate(
    pitch_type = 'changeup'
  )

changeuprapsodo2 <- read_csv("changeuprapsodo2.csv")
changeuprapsodo2 <- changeuprapsodo2 %>%
  mutate(
    pitch_type = 'changeup'
  )

splitterrapsodo <- read_csv("splitterrapsodo.csv")
splitterrapsodo <- splitterrapsodo %>%
  mutate(
    pitch_type = 'splitter'
  )

rapsodofeb6 <- bind_rows(fastballrapsodo, fastballrapsodo2, fastballrapsodo3,
                         cutterrapsodo, curveballrapsodo, sliderrapsodo, sliderrapsodo2,
                         twoseamrapsodo, changeuprapsodo, changeuprapsodo2, splitterrapsodo)

rapsodofeb6 <- rapsodofeb6 %>%
  clean_names()

rapsodofeb6 <- rapsodofeb6 %>%
  distinct()

rapsodofeb6 <- rapsodofeb6 %>%
  mutate(
    pitch_group = case_when(
      pitch_type %in% c(
        "fastball", "twoseamfastball", "cutter"
      ) ~ "ff",
      
      pitch_type %in% c(
        "slider", "curveball"
      ) ~ "bb",
      
      pitch_type %in% c(
        "changeup", "splitter"
      ) ~ "off",
      
      TRUE ~ NA_character_
    )
  )

str(rapsodofeb6)
View(rapsodofeb6)

rapsodofeb6 <- rapsodofeb6[-52, ]
new_row <- tibble(
  player = "Austin Stone",
  age = 22,
  velocity_max = 76.0,
  velocity_avg = 72.9,
  strike_percent = 21,
  spin_avg = 1219,
  spin_dir_avg = NA,
  spin_eff_avg = 72.5,
  total_break = 15.8,
  horz_break = 8.7,
  vert_break = 7.2,
  pitch_type = "changeup",
  pitch_group = "off"
)
rapsodofeb6 <- bind_rows(rapsodofeb6, new_row)


check <- pitchinggroup %>%
  group_by(player_name, pitch_type) %>%
  summarise(
    mean_ivb = mean(vb_spin, na.rm = TRUE),
    mean_hb_spin  = mean(hb_spin,  na.rm = TRUE),
    mean_vb = mean(vb_trajectory, na.rm = TRUE),
    mean_hb = mean(hb_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
View(check)

rapsodo_clean <- rapsodofeb6 %>%
  mutate(
    hb = -horz_break,
    vb = vert_break
  )

rapsodo_clean <- rapsodo_clean %>%
  select(-horz_break, -vert_break, -total_break)
view(rapsodo_clean)

rapsodo_grouped <- rapsodo_clean %>%
  group_by(player, pitch_group) %>%
  summarise(
    vb  = mean(vb,  na.rm = TRUE),
    hb   = mean(hb,   na.rm = TRUE),
    velo = mean(velocity_avg, na.rm = TRUE),
    spin = mean(spin_avg, na.rm = TRUE),
    .groups = "drop"
  )
View(rapsodo_grouped)

rapsodo_grouped <- rapsodo_grouped %>%
  rename(
    Velocity = velo
  )

rapsodo_grouped <- rapsodo_grouped %>%
  pivot_wider(
    names_from = pitch_group,
    values_from = c(Velocity, spin, hb, vb),
    names_glue = "{pitch_group}_{.value}"
  )

rapsodo_grouped <- rapsodo_grouped %>%
  mutate(across(all_of(predictor_cols_shape), ~ replace_na(., 0)))

### using check table the new data imported is closest to trajectory break and not spin break

##################################################################

statcastmetricsmlb <- read_csv("statcastmetricsmlb.csv")
View(statcastmetricsmlb)

statcastmetricsmlb <- statcastmetricsmlb %>%
  dplyr::rename(ff_Velocity = `fastball_avg_speed`)

statcastmetricsmlb <- statcastmetricsmlb %>%
  dplyr::rename(bb_Velocity = `breaking_avg_speed`)

statcastmetricsmlb <- statcastmetricsmlb %>%
  dplyr::rename(off_Velocity = `offspeed_avg_speed`)

statcastmetricsmlb <- statcastmetricsmlb %>%
  dplyr::rename(ff_spin = `fastball_avg_spin`)

statcastmetricsmlb <- statcastmetricsmlb %>%
  dplyr::rename(off_spin = `offspeed_avg_spin`)

statcastmetricsmlb <- statcastmetricsmlb %>%
  dplyr::rename(bb_spin = `breaking_avg_spin`)

statcastmetricsmlb <- statcastmetricsmlb %>%
  dplyr::rename(ff_hb = fastball_avg_break_x)

statcastmetricsmlb <- statcastmetricsmlb %>%
  dplyr::rename(off_hb = offspeed_avg_break_x)

statcastmetricsmlb <- statcastmetricsmlb %>%
  dplyr::rename(bb_hb = breaking_avg_break_x)

statcastmetricsmlb <- statcastmetricsmlb %>%
  dplyr::rename(ff_vb = fastball_avg_break_z_induced)

statcastmetricsmlb <- statcastmetricsmlb %>%
  dplyr::rename(off_vb = offspeed_avg_break_z_induced)

statcastmetricsmlb <- statcastmetricsmlb %>%
  dplyr::rename(bb_vb = breaking_avg_break_z_induced)

statcastmetricsmlb <- statcastmetricsmlb %>%
  dplyr::rename(ff_usage_pct = n_fastball_formatted)

statcastmetricsmlb <- statcastmetricsmlb %>%
  dplyr::rename(bb_usage_pct = n_breaking_formatted)

statcastmetricsmlb <- statcastmetricsmlb %>%
  dplyr::rename(off_usage_pct = n_offspeed_formatted)

statcastmetricsmlb <- statcastmetricsmlb %>%
  select(-offspeed_avg_break_z)


statcastmetricsmlb <- statcastmetricsmlb %>%
  mutate(
    hr_9 = round(home_run / p_formatted_ip * 9, 3)
  )

View(statcastmetricsmlb)

##################################################################

### model creation

##################################################################

target_metrics <- c('xba',
                    'hard_hit_percent',
                    'xiso',
                    'xobp',
                    'k_percent',
                    'whiff_percent',
                    'groundballs_percent',
                    'flyballs_percent'
)

predictor_cols <- statcastmetricsmlb %>%
  select(
    starts_with("ff_"),
    starts_with("bb_"),
    starts_with("off_")
  ) %>%
  colnames()

predictor_cols_shape <- c(
  "ff_Velocity", "ff_spin", "ff_hb", "ff_vb",
  "bb_Velocity", "bb_spin", "bb_hb", "bb_vb",
  "off_Velocity", "off_spin", "off_hb", "off_vb"
)


library(glmnet)

final_models_shape <- list()

for (target in target_metrics) {
  
  train_complete <- train_data %>%
    select(all_of(c(target, predictor_cols_shape))) %>%
    drop_na()
  
  X <- model.matrix(
    as.formula(paste(target, "~ .")),
    train_complete
  )[, -1]
  
  y <- train_complete[[target]]
  
  ridge_fit <- cv.glmnet(
    x = X,
    y = y,
    alpha = 0,
    nfolds = 10
  )
  
  final_models_shape[[target]] <- ridge_fit
}

names(final_models_shape)
final_models_shape[["whiff_percent"]]

projections <- list()

X_new <- model.matrix(
  ~ .,
  rapsodo_grouped %>% select(all_of(predictor_cols_shape))
)[, -1]

projections <- list()

for (target in names(final_models_shape)) {
  
  model_obj <- final_models_shape[[target]]
  
  preds <- predict(
    model_obj,
    newx = X_new,
    s = "lambda.1se"
  )
  
  projections[[target]] <- as.vector(preds)
}

projection_df <- bind_cols(
  rapsodo_grouped %>% select(player),
  as_tibble(projections)
)

View(projection_df)

mlb_means <- train_data %>% summarise(across(all_of(predictor_cols_shape), mean, na.rm = TRUE))
d3_means  <- rapsodo_grouped %>% summarise(across(all_of(predictor_cols_shape), mean, na.rm = TRUE))

adjustment <- mlb_means - d3_means

rapsodo_adjusted <- rapsodo_grouped
for(col in predictor_cols_shape){
  rapsodo_adjusted[[col]] <- rapsodo_adjusted[[col]] + adjustment[[col]]
}

View(rapsodo_adjusted)

X_new2 <- model.matrix(
  ~ .,
  rapsodo_adjusted %>% select(all_of(predictor_cols_shape))
)[, -1]

projections2 <- list()

for (target in names(final_models_shape)) {
  
  model_obj <- final_models_shape[[target]]
  
  preds <- predict(
    model_obj,
    newx = X_new2,
    s = "lambda.1se"
  )
  
  projections2[[target]] <- as.vector(preds)
}

projection_adjusted <- bind_cols(
  rapsodo_adjusted %>% select(player),
  as_tibble(projections2)
)

View(projection_adjusted)

write.csv(projection_adjusted, 'projection_adjusted.csv', row.names = FALSE)

##################################################################
