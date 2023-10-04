library(tidyverse)
library(vroom)
library(tidymodels)
library(glmnet)
library(poissonreg)
library(rpart)
library(ranger)
library(stacks)
library(parsnip)
library(dbarts)
setwd("./KaggleBikeShare")

#read in csvs
bikeTrain <- vroom("./train.csv")
bikeTest <- vroom("test.csv")

#remove casual and registered from train, not included in test
bikeTrain <- bikeTrain %>%
  select(-casual, -registered)

#create recipe for data (both train and test)
my_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_num2factor(weather, levels=c("Sunny", "Mist", "Rain")) %>%
  step_num2factor(season, levels=c("Spring", "Summer", "Fall", "Winter")) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
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


bikeTrain <- vroom("./train.csv")
bikeTest <- vroom("test.csv")

bikeTrain <- bikeTrain %>%
  select(-casual, -registered)

#do a linear regression on log counts
#penalized regression on log count
my_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_num2factor(weather, levels=c("Sunny", "Mist", "Rain")) %>%
  step_num2factor(season, levels=c("Spring", "Summer", "Fall", "Winter")) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

preg_model <- linear_reg(penalty = 4, mixture = .5) %>%
  set_engine("glmnet")
preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model) %>%
  fit(data = bikeTrain)
bike_predictions <- predict(preg_wf, new_data = bikeTest)
bike_predictions$.pred[is.na(bike_predictions$.pred)] <- 0
bike_predictions$.pred[bike_predictions$.pred < 0] <- 0

#bind predictions and datetime column, format for kaggle
preds <- cbind(bikeTest$datetime, bike_predictions)
colnames(preds) <- c("datetime","count")
preds$datetime <- as.character(format(preds$datetime))

vroom_write(preds, "penalty_predictions.csv", ",")




bikeTrain <- vroom("./train.csv")
bikeTest <- vroom("test.csv")

bikeTrain <- bikeTrain %>%
  select(-casual, -registered)

#bikeTrain$day <- wday(bikeTrain$datetime)
#bikeTest$day <- wday(bikeTest$datetime)

logBikeTrain <- bikeTrain %>%
  mutate(count = log(count))

bike_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(weekend = ifelse((workingday == 0 & holiday == 0), 1, 0)) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(weekend=factor(weekend, levels = c(0,1), labels = c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(workingday, datetime, atemp) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

pois_mod <- poisson_reg() %>%
  set_engine("glm")
bike_pois_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(pois_mod) %>%
  fit(data = bikeTrain)
pois_bike_preds <- predict(bike_pois_workflow, new_data = bikeTest)

preg_model <- poisson_reg(penalty = 5, mixture = .8) %>%
  set_engine("glm")
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model) %>%
  fit(data = bikeTrain)
preg_bike_preds <- predict(preg_wf, new_data = bikeTest)

preds_function <- function(bike_predictions) {
  bike_predictions$.pred[is.na(bike_predictions$.pred)] <- 0
  bike_predictions$.pred[bike_predictions$.pred < 0] <- 0
  preds <- cbind(bikeTest$datetime, bike_predictions)
  colnames(preds) <- c("datetime","count")
  preds$datetime <- as.character(format(preds$datetime))
  #preds$count <- exp(preds$count)
  
  return(preds)
}
pois_preds <- preds_function(pois_bike_preds)
preg_preds <- preds_function(preg_bike_preds)

vroom_write(pois_preds, "pois_predictions.csv", ",")
vroom_write(preg_preds, "preg_predictions.csv", ",")



bikeTrain <- vroom("./train.csv")
bikeTest <- vroom("test.csv")

bikeTrain <- bikeTrain %>%
  select(-casual, -registered)

bikeTrain <- bikeTrain %>%
  mutate(count = log(count))

bike_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(weekend = ifelse((workingday == 0 & holiday == 0), 1, 0)) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(weekend=factor(weekend, levels = c(0,1), labels = c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_date(datetime, features="year") %>%
  step_rm(workingday, datetime, atemp) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

my_mod <- rand_forest(mtry = tune(),
                      min_n = tune(),
                      trees=500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rand_forest_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(my_mod)

tuning_grid <- grid_regular(mtry(range = c(1,9)),
                            min_n(),
                            levels = 5)

folds <- vfold_cv(bikeTrain, v = 5, repeats = 1)

CV_results <- rand_forest_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(rmse, mae, rsq))

bestTune <- CV_results %>%
  select_best("rmse")

final_wf <- rand_forest_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = bikeTrain)

preds <- final_wf %>%
  predict(new_data = bikeTest)

preds_function <- function(bike_predictions) {
  bike_predictions$.pred[is.na(bike_predictions$.pred)] <- 0
  bike_predictions$.pred[bike_predictions$.pred < 0] <- 0
  preds <- cbind(bikeTest$datetime, bike_predictions)
  colnames(preds) <- c("datetime","count")
  preds$datetime <- as.character(format(preds$datetime))
  preds$count <- exp(preds$count)
  
  return(preds)
}

final_preds <- preds_function(preds)
vroom_write(final_preds, "final_predictions.csv", ",")

bikeTrain <- vroom("./train.csv")
bikeTest <- vroom("test.csv")

bikeTrain <- bikeTrain %>%
  select(-casual, -registered)

bikeTrain <- bikeTrain %>%
  mutate(count = log(count))

bike_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(weekend = ifelse((workingday == 0 & holiday == 0), 1, 0)) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(weekend=factor(weekend, levels = c(0,1), labels = c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_date(datetime, features="year") %>%
  step_rm(workingday, datetime, atemp) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

bart_mod <- parsnip::bart(mode = "regression",engine = "dbarts", trees = 50)
bart_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(bart_mod)

tuning_grid <- grid_regular(mtry(range = c(1,9)),
                            min_n(),
                            levels = 5)

folds <- vfold_cv(bikeTrain, v = 5, repeats = 1)

CV_results <- bart_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(rmse, mae, rsq))

bestTune <- CV_results %>%
  select_best("rmse")

final_wf <- bart_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = bikeTrain)

preds <- final_wf %>%
  predict(new_data = bikeTest)

preds_function <- function(bike_predictions) {
  bike_predictions$.pred[is.na(bike_predictions$.pred)] <- 0
  bike_predictions$.pred[bike_predictions$.pred < 0] <- 0
  preds <- cbind(bikeTest$datetime, bike_predictions)
  colnames(preds) <- c("datetime","count")
  preds$datetime <- as.character(format(preds$datetime))
  preds$count <- exp(preds$count)
  
  return(preds)
}

bart_preds <- preds_function(preds)
vroom_write(bart_preds, "bart_predictions.csv", ",")


#model stacking
untunedModel <- control_stack_grid()
tunedModel <- control_stack_resamples()
preg_model <- linear_reg(penalty=tune(), mixture=tune()) %>%
  set_engine("glmnet")
preg_wf <- workflow() %>%
  add_recipe(bikeRecipe) %>%
  add_model(preg_model)
preg_tuning_grid <- grid_regular(penalty(),
                                 mixture(),
                                 levels = 5)
preg_models <- preg_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(rmse, mae, rsq),
            control=untunedModel)
lin_reg <-
  linear_reg() %>%
  set_engine("lm")
lin_reg_wf <- workflow() %>%
  add_model(lin_reg_spec) %>%
  add_recipe(lin_reg_rec)
lin_reg_model <- fit_resample(lin_reg_wf,
                              resamples = folds,
                              metrics = metric,
                              control = tunedModel)
pois_mod <- poisson_reg() %>%
  set_engine("glm")
bike_pois_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(pois_mod) %>%
  fit(data = bikeTrain)

my_stack <- stacks() %>%
  add_candidates(preg_models) %>%
  add_candidates(lin_reg_model) %>%
  add_candidates(bike_pois_workflow)
stack_mod <- my_stack %>%
  blend_predictions() %>%
  fit_members()
stackData <- as_tibble(my_stack)
preds <- stack_mod %>% predict(new_data=bikeTest)
preds_function <- function(bike_predictions) {
  bike_predictions$.pred[is.na(bike_predictions$.pred)] <- 0
  bike_predictions$.pred[bike_predictions$.pred < 0] <- 0
  preds <- cbind(bikeTest$datetime, bike_predictions)
  colnames(preds) <- c("datetime","count")
  preds$datetime <- as.character(format(preds$datetime))
  preds$count <- exp(preds$count)
  
  return(preds)
}

final_preds <- preds_function(preds)
vroom_write(final_preds, "stackedmodel_preds.csv")

