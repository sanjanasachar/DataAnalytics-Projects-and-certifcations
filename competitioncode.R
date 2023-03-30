#Load required packages
library(randomForest)

# Read in training data
train_data <- read.csv("Training_NBA players.csv")

# Convert Shooting Efficiency to numeric
train_data$shooting_Efficiency <- as.numeric(train_data$shooting_Efficiency)

# Create a binary shooting efficiency variable
train_data$high_Efficiency <- ifelse(train_data$shooting_Efficiency >= median(train_data$shooting_Efficiency, na.rm = TRUE), 1, 0)

# Read in validation data
valid_data <- read.csv("Validation_NBA Players.csv")

# Convert Shooting Efficiency to numeric
valid_data$shooting_Efficiency <- as.numeric(valid_data$shooting_Efficiency)

# Create a binary shooting efficiency variable
valid_data$high_Efficiency <- ifelse(valid_data$shooting_Efficiency >= median(train_data$shooting_Efficiency, na.rm = TRUE), 1, 0)

# Build random forest model
set.seed(123)
rf_model <- randomForest(high_Efficiency ~ age + player_height + player_weight + gp + reb + ast + net_rating + oreb_pct + dreb_pct + usg_pct + ast_pct, data=train_data, importance=TRUE, proximity=TRUE)

# Make predictions on validation data
valid_pred <- predict(rf_model, newdata=valid_data, type="class")

# Calculate accuracy
accuracy <- sum(valid_pred == valid_data$high_Efficiency) / nrow(valid_data)
cat("Prediction accuracy:", round(accuracy, 2), "%")

