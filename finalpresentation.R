#Load required packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
# Load dataset
airlines <- read.csv("airlines.csv")

# Rename columns to remove spaces
names(airlines) <- c("Satisfaction", "Gender", "CustomerType", "Age", "TypeofTravel", "Class", 
                     "FlightDistance", "SeatComfort", "DepartureArrivalTimeConvenient", "FoodandDrink", 
                     "GateLocation", "InflightWifiService", "InflightEntertainment", "OnlineSupport", 
                     "EaseofOnlineBooking", "OnboardService", "LegRoomService", "BaggageHandling", 
                     "CheckinService", "Cleanliness", "OnlineBoarding", "DepartureDelayinMinutes", 
                     "ArrivalDelayinMinutes")

# Remove irrelevant columns
airlines <- airlines %>%
  select(-c(Gender, FlightDistance, DepartureDelayinMinutes, ArrivalDelayinMinutes))

# Handle missing values
airlines <- airlines %>%
  drop_na()

# Convert categorical variables to factors
airlines$Satisfaction <- as.factor(airlines$Satisfaction)
airlines$Class <- as.factor(airlines$Class)

# Visualization 1: Distribution of satisfaction
ggplot(airlines, aes(x = Satisfaction)) +
  geom_bar() +
  labs(title = "Distribution of Satisfaction")

# Visualization 2: Correlation between satisfaction and seat comfort
ggplot(airlines, aes(x = Satisfaction, y = SeatComfort)) +
  geom_boxplot() +
  labs(title = "Correlation between Satisfaction and Seat Comfort")

# Visualization 3: Proportion of satisfied customers by class
airlines %>%
  group_by(Class, Satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count)) %>%
  filter(Satisfaction == "satisfied") %>%
  ggplot(aes(x = Class, y = percentage, fill = Class)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Satisfied Customers by Class")

# CART analysis
set.seed(123)
cart_model <- rpart(Satisfaction ~ SeatComfort, data = airlines, method = "class")
rpart.plot(cart_model)
