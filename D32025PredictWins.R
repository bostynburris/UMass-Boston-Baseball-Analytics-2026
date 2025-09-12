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

d32025teamtotals$XBH <- d32025teamtotals$X2B + d32025teamtotals$X3B + d32025teamtotals$HR.x

exclude_columns <- c("Tm", "L", "G.x", "G.y", "W.L.", "GS", "GF")
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

train_scaled$Wins <- train_data$W
test_scaled$Wins <- test_data$W
train_scaled <- train_scaled[, -25]
test_scaled <- test_scaled [, -25]

head(train_scaled)
head(test_scaled)

exclude_columns <- c("W", "Tm", "L", "G.x", "G.y", "W.L.", "GS", "GF")
predictor_columns <- setdiff(names(d32025teamtotals), exclude_columns)

model <- lm(Wins ~ R.G.x + OBP + SLG + ERA + WHIP + SO9 + HR9 + BB9, data = train_scaled)
summary(model)
install.packages("stargazer")
library(stargazer)

stargazer(model, type = "text")

model2 <- lm(Wins ~ R.G.x +  SLG + WHIP + BB9, data = train_scaled)
summary(model2)

stargazer(model, type = "text")

library(glmnet)

predictor_data_split <- subset(predictor_data, split == TRUE)
predictor_data_split <- predictor_data_split[, -25]

X <- as.matrix(predictor_data_split)
y <- train_scaled$Wins

lassomodel <- cv.glmnet(X, y, alpha = 1)
coef(lassomodel, s = "lambda.min")

drop_cols_lasso <- c("R.G.x", "R.G.y", "R.x", "RBI", "OPS", "RA9", "ERA", "PA",
                     "BA", "SHO", "SV", "CG","HBP.y", "AB", "BK", "WP", "TB", "SH", "SF", "SLG", "OBP", "BA", 
                     "SO9", "SO.y", "BB.y", "BF", "HR.y", "BB9", "HR.x")
X_cleaned <- predictor_data_split[, !(names(predictor_data_split) %in% drop_cols_lasso)]

X_cleaned <- as.matrix(X_cleaned)
lassomodel2 <- cv.glmnet(X_cleaned, y, alpha =1, nfolds. = 10, standardize = TRUE)
coef(lassomodel2, s = "lambda.min")

lassovariables <- c("H.x", "X2B", "X3B", "SB", "CS", "BB.x", "SO.x", "GDP",
                    "HBP.x", "IBB.x", "IP", "H.y", "R.y", "ER", "IBB.y",
                    "WHIP", "H9", "HR9", "SO.W", "XBH")

formula <- as.formula(paste("Wins ~", paste(lassovariables, collapse = " + ")))
model3 <- lm(formula, data = train_scaled)
summary(model3)
stagrazer(model3)

y_pred <- predict(model3, newdata = test_scaled)
y_test <- subset(predictor_data$W, split == FALSE)

mse <- mean((y_test - y_pred)^2)
r_squared <- cor(y_test, y_pred)^2
rmse <- sqrt(mse)
cat(rmse)
cat(r_squared)

plot <- data.frame(Actual = y_test, Predicted = y_pred)
summary(plot)
str(plot)

library(ggplot2)
ggplot(plot, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Actual vs Predicted Wins", x = "Actual Wins", y = "Predicted Wins") +
  theme_minimal()

UMBBoston2025 <- data.frame(H.x = 425,
                            X2B = 83,
                            X3B = 14,
                            SB = 116,
                            CS = 20,
                            BB.x = 194,
                            SO.x = 259,
                            GDP = 8,
                            HBP.x = 89,
                            IBB.x = 5,
                            IP = 364.1,
                            H.y = 377,
                            R.y = 229,
                            ER = 204,
                            IBB.y = 5,
                            WHIP = 1.570,
                            H9 = 9.3,
                            HR9 = 0.4,
                            SO.W = 1.81,
                            XBH = 117
)
View(UMBBoston2025)

# Scale using the training means and sds
UMBBoston2025_scaled <- UMBBoston2025
UMBBoston2025_scaled[lassovariables] <- sweep(UMBBoston2025_scaled[lassovariables], 2, train_means[lassovariables], "-")
UMBBoston2025_scaled[lassovariables] <- sweep(UMBBoston2025_scaled[lassovariables], 2, train_sds[lassovariables], "/")

predicted_UMBBoston2025 <- predict(model3, newdata = UMBBoston2025_scaled)
print(predicted_UMBBoston2025)
