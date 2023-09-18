library(tidyverse)
library(vroom)
library(tidymodels)
setwd("./KaggleBikeShare")

#read in csvs
bikeTrain <- vroom("./train.csv")
bikeTest <- vroom("test.csv")

#remove casual and registered from train, not included in test
bikeTrain <- bikeTrain %>%
  select(-casual, -registered)

#create recipe for data (both train and test)
my_recipe <- recipe(count ~ ., data = bikeTrain) %>%
  step_mutate(weather = ifelse(weather == 4,3,weather)) %>%
  step_num2factor(weather, levels = c('1','2','3','4')) %>% #change to factor
  step_num2factor(season, levels = c('0','1')) %>% #change to factor
  step_num2factor(holiday, levels = c('0','1'), transform = function(x) x + 1) %>% #change to factor
  step_num2factor(workingday, levels = c('0','1'), transform = function(x) x + 1) %>% #change to factor
  step_time(datetime, features = "hour") %>% #get hour of day
  step_rm(datetime)
  
prepped_recipe <- prep(my_recipe)
baked <- bake(prepped_recipe, new_data = bikeTrain)

my_mod <- linear_reg() %>%
  set_engine("lm")

#set up the workflow
bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod) %>%
  fit(data = bikeTrain)

#get predictions for test set
bike_predictions <- predict(bike_workflow, new_data = bikeTest)

#format for kaggle
bike_predictions$.pred[is.na(bike_predictions$.pred)] <- 0
bike_predictions$.pred[bike_predictions$.pred < 0] <- 0

#bind predictions and datetime column, format for kaggle
preds <- cbind(bikeTest$datetime, bike_predictions)
colnames(preds) <- c("datetime","count")
preds$datetime <- as.character(format(preds$datetime))

#write to csv
vroom_write(preds, "predictions.csv", ",")

#look at the fitted LM model here
extract_fit_engine(bike_workflow) %>%
  tidy()

#fit a poisson model
library(poissonreg)

pois_mod <- poisson_reg() %>%
  set_engine("glm")

bike_pois_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(pois_mod) %>%
  fit(data = bikeTrain)

bike_predictions <- predict(bike_pois_workflow,
                            new_data = bikeTest)

bike_predictions$.pred[is.na(bike_predictions$.pred)] <- 0
bike_predictions$.pred[bike_predictions$.pred < 0] <- 0

#bind predictions and datetime column, format for kaggle
preds <- cbind(bikeTest$datetime, bike_predictions)
colnames(preds) <- c("datetime","count")
preds$datetime <- as.character(format(preds$datetime))

#write to csv
vroom_write(preds, "pois_predictions.csv", ",")
