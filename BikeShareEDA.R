##
## Bike Share EDA Code
##

## Libraries
library(tidyverse)
library(vroom)
library(patchwork)
library(DataExplorer)
library(tidymodels)
setwd("./KaggleBikeShare")
## Read in the Data
bike <- vroom("./train.csv")

## EDA
#make columns factors not numeric
bike$dayofweek <- wday(bike$datetime)
bike$hour <- hour(bike$datetime)
bike <- bike %>%
  mutate_at(c("season","holiday","workingday","weather","dayofweek","hour"), factor)
bike <- subset(bike, select = -c(casual, registered, datetime)) %>%
  select(-count, count)






plot_correlation(bike)
plot_bar(bike)

season_plot <- ggplot() +
  geom_boxplot(data = bike, aes(x = season, y = count)) +
  ggtitle("Seasonal Bike Count")

weather_plot <- ggplot() +
  geom_boxplot(data = bike, aes(x = weather, y = count)) +
  ggtitle("Bike Count by Weather Type")

temp_plot <- ggplot(data = bike, aes(x = temp, y = count)) +
  geom_smooth(se = FALSE) +
  ggtitle("Bike Count by Temperature")

humid_plot <- ggplot(data = bike, aes(x = humidity, y = count)) +
  geom_smooth(se = FALSE) +
  ggtitle("Bike Count by Humidity")

ggsave("4plotsEDA.png", (season_plot + weather_plot) / (temp_plot + humid_plot))
ggsave()