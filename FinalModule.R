rm(list = ls())
NBA<-read_csv('nba_team_stats_00_to_23.csv')
library(rpart)
library(rpart.plot)
library(tidyverse)
library(forecast)
NBA<-NBA%>%select(-teamstatspk,-Team,games_played,-wins,-losses,-Min,-points,-field_goals_made,
                  -field_goals_attempted,-three_pointers_made,-three_pointers_attempted,-free_throws_made,
                  -free_throw_attempted,-offensive_rebounds,-defensive_rebounds,-rebounds,-assists,
                  -turnovers,-steals,-blocks,-blocks_attempted,-personal_fouls,-personal_fouls_drawn,-season)
NBA<-NBA%>%select(-games_played)
set.seed(1)
Train <- NBA %>% sample_frac(0.8) 
Testing <- NBA %>% anti_join(Train)
Tree <- rpart(win_percentage~., data=Train, method="anova")
rpart.plot(Tree)
Train.Predictions <- predict(Tree)
train_rmse <- sqrt(mean((Train$win_percentage - Train.Predictions)^2))
Test.Predictions <- predict(Tree, newdata = Testing)
test_rmse <- sqrt(mean((Testing$win_percentage - Test.Predictions)^2))
cat("Training RMSE:", train_rmse, "\n")
cat("Testing RMSE:", test_rmse, "\n")

library(ggplot2)


plot_data <- data.frame(
  Actual = c(Train$win_percentage, Testing$win_percentage),
  Predicted = c(Train.Predictions, Test.Predictions),
  Set = c(rep("Training", nrow(Train)), rep("Testing", nrow(Testing)))
)

ggplot(plot_data, aes(x = Actual, y = Predicted, color = Set)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  facet_wrap(~Set) +
  labs(title = "Actual vs Predicted Win Percentage",
       x = "Actual Win Percentage",
       y = "Predicted Win Percentage") +
  theme_minimal()
