library(tidyverse)
library(vroom)
library(tidymodels)
setwd("./KaggleBikeShare")

bike <- vroom("./train.csv")
new <- vroom("test.csv")

bike <- bike %>%
  mutate(weather = ifelse(weather == 4,3,weather)) %>%
  select(-casual, -registered)

#perform at least 2 feature engineering steps using recipe (change to factors, create time of day variable)

my_recipe <- recipe(count ~ ., data = bike) %>%
  step_num2factor(weather, levels = c('1','2','3','4')) %>% #change to factor
  step_num2factor(season, levels = c('0','1')) %>% #change to factor
  step_num2factor(holiday, levels = c('0','1'), transform = function(x) x + 1) %>% #change to factor
  step_num2factor(workingday, levels = c('0','1'), transform = function(x) x + 1) %>% #change to factor
  step_date(datetime, features = "dow") %>% #get day of the week
  step_time(datetime, features = "hour") %>% #get hour of day
  step_zv(all_predictors()) %>%
  step_rm(datetime)
  
prepped_recipe <- prep(my_recipe)
baked <- bake(prepped_recipe, new_data = bike)
head(baked, 10)
