library(tidyverse)
library(vroom)
library(tidymodels)
setwd("./KaggleBikeShare")

bike <- vroom("./train.csv")
new <- vroom("test.csv")

bike <- bike %>%
  select(-casual, -registered)

#perform at least 2 feature engineering steps using recipe (change to factors, create time of day variable)

my_recipe <- recipe(count ~ ., data = bike) %>%
  step_mutate(weather = ifelse(weather == 4,3,weather)) %>%
  step_num2factor(weather, levels = c('1','2','3','4')) %>% #change to factor
  step_num2factor(season, levels = c('0','1')) %>% #change to factor
  step_num2factor(holiday, levels = c('0','1'), transform = function(x) x + 1) %>% #change to factor
  step_num2factor(workingday, levels = c('0','1'), transform = function(x) x + 1) %>% #change to factor
  step_time(datetime, features = "hour") #get hour of day
  
prepped_recipe <- prep(my_recipe)
baked <- bake(prepped_recipe, new_data = bike)

my_mod <- linear_reg() %>%
  set_engine("lm")

bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod) %>%
  fit(data = bike)

bike_predictions <- predict(bike_workflow, new_data = new)

bike_predictions$.pred[is.na(bike_predictions$.pred)] <- 0
bike_predictions$.pred[bike_predictions$.pred < 0] <- 0
preds <- cbind(new$datetime, bike_predictions)
colnames(preds) <- c("datetime","count")

vroom_write(preds, "predictions.csv", ",")
