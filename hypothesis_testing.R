library(sf)
library(raster)
library(dplyr)
library(tidyverse)
library(fs)
library(rgdal)

covid_speed <- read.csv("covid_speed.csv")

# Hypothesis testing with multiple variable regression

# Here I use the data, proposed by the first method 
# (no population density scaling)
data <- read.csv("country_averaged.csv")
data <- data[data$Country %in% covid_speed$Country, ]
fit <- lm(covid_speed$Ratio ~ data$Temperature + data$Humidity + data$Windspeed)
print(summary(fit))


# Here I use the data, proposed by the second method 
# (population density scaling)
data <- read.csv("country_density_scaled.csv")
data <- data[data$Country %in% covid_speed$Country, ]
fit <- lm(covid_speed$Ratio ~ data$Temperature + data$Humidity + data$Windspeed)
print(summary(fit))
