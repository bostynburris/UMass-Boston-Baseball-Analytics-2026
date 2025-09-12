library(readr)
library(readxl)

d3teamoffensivestats <- data.frame(read_xlsx("/Users/bostynburris/Desktop/D32025TeamData/D32025TeamHittingTotals.xlsx"))
d3teamdefensivestats <- data.frame(read_xlsx("/Users/bostynburris/Desktop/D32025TeamData/D32025TeamPitchingTotals.xlsx"))

View(d3teamoffensivestats)
View(d3teamdefensivestats)

library(dplyr)

d32025teamtotals <- full_join(d3teamoffensivestats, d3teamdefensivestats, by = "Tm")
View(d32025teamtotals)

d32025teamtotals <- d32025teamtotals[, colSums(is.na(d32025teamtotals)) < nrow(d32025teamtotals)]
d32025teamtotals <- d32025teamtotals[, !names(d32025teamtotals) %in% c("BatAge", "PAge")]

sum(is.na(d32025teamtotals))

exclude_columns <- c("W", "Tm", "L", "G.x", "G.y", "W.L.", "GS", "GF")
predictor_columns <- setdiff(names(d32025teamtotals), exclude_columns)

predictor_data <- d32025teamtotals %>% select(-all_of(exclude_columns))
View(predictor_data)

library(caTools)

set.seed(1)
split <- sample.split(d32025teamtotals$W, SplitRatio = 0.8)

train_data <- subset(predictor_data, split == TRUE)
test_data <- subset(predictor_data, split == FALSE)

train_means <- sapply(train_data[predictor_columns], mean)
train_sds   <- sapply(train_data[predictor_columns], sd)

head(train_means)
head(train_sds)

train_scaled <- train_data
train_scaled[predictor_columns] <- sweep(train_scaled[predictor_columns], 2, train_means, "-")
train_scaled[predictor_columns] <- sweep(train_scaled[predictor_columns], 2, train_sds, "/")

test_scaled <- test_data
test_scaled[predictor_columns] <- sweep(test_scaled[predictor_columns], 2, train_means, "-")
test_scaled[predictor_columns] <- sweep(test_scaled[predictor_columns], 2, train_sds, "/")

head(train_scaled)
head(test_scaled)

formula <- as.formula(paste("W ~", paste(predictor_data, collapse = " + ")))

model <- lm(formula, data = train_scaled)
