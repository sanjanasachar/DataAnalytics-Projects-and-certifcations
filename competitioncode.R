library(randomForest)

# read in training data
train_data <- read.csv("Training_NBA players.csv")

# shooting efficiency to binary variable
train_data$shooting_Efficiency <- ifelse(train_data$shooting_Efficiency > 0, 1, 0)

# read in validation data
valid_data <- read.csv("Validation_NBA Players.csv")

# shooting efficiency to binary variable
valid_data$shooting_Efficiency <- ifelse(valid_data$shooting_Efficiency > 0, 1, 0)

# create random forest model
set.seed(100)
rf_model <- randomForest(shooting_Efficiency ~ net_rating + reb + oreb_pct + ast + player_height, data=train_data, importance=TRUE, proximity=TRUE)

plot(train_data$net_rating, train_data$shooting_Efficiency)

# create predictions based on model
valid_pred <- predict(rf_model, newdata=valid_data, class=TRUE)
valid_pred <- ifelse(valid_pred > median(valid_pred), 1, 0)

# check accuracy
accuracy <- mean(valid_pred == valid_data$shooting_Efficiency) * 100
print(paste0("Accuracy: ", round(accuracy, 2), "%"))
