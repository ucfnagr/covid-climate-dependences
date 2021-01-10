library(sf)
library(dplyr)
library(tidyverse)
library(fs)
library(rgdal)

# covidDF - data from covid-19 dataset
# contains information about number of confirmed cases
# in each country for all days from January, 22 of 2020 year
covidDF <- read.csv("covid.csv")

# Europe countries dataset
europe <- st_read("geodata\\Europe\\europe.shp")
countries <- europe$NAME

# several items in "countries" are actually belong
# to another country, so I exclude them
removing <- c("Gibraltar (UK)", "Guernsey (UK)",
              "Isle of Man (UK)", "Jersey (UK)",
              "Faeroe Islands (Denmark)", "Svalbard (Norway)",
              "Jan Mayen (Norway)")
countries <- countries[!countries %in% removing]
# several countries are named differently in Europe countries
# dataset and COVID-19 dataset, they need to be renamed
countries <- countries %>%
  replace(., countries=="Bosnia Herzegovina", "Bosnia and Herzegovina") %>%
  replace(., countries=="Czech Republic", "Czechia") %>%
  replace(., countries=="Macedonia", "North Macedonia")

# the function which computes the speed of the COVID-19 spread
# takes the dataframe with date-country-confirmed items
# returns the single value - speed of the spread, which is
# actually the base of exponent function, determing the growth law
compute_exp_base <- function(dataframe){
  # I consider only dates, when the number of confirmed
  # cases is greater than 100 (for smaller values 
  # the function is hardly approximated by the exponent
  # I also consider only first four months as it is stated in
  # the research
  dataframe <- dataframe %>%
    filter(., Confirmed > 100) %>%
    filter(., Date < "2020-05-01")
  conf <- dataframe$Confirmed
  # I exclude countries, which do not have enough
  # dates to approximate the spread speed
  if (length(conf) <= 5){
    return (0)
  }
  
  # Here I compute the base of the exponent the way
  # described in the research
  base <- 0
  for (i in c(6:length(conf))){
    base <- base + sum(conf[(i-4):i]) / sum(conf[(i-5):(i-1)])
  }
  base <- base / (length(conf) - 5)
  return (base)
}


# create the dataframe and compute the spread speed for each country
covid_speed <- data.frame(countries, rep(0, length(countries)))
names(covid_speed) <- c("Country", "Ratio")
counter <- 1
for (country in countries){
  covid_speed[counter, 2] <- compute_exp_base(filter(covidDF, Country==country))
  counter <- counter + 1
}

# replace back countries with different names
# and exclude countries which lack the data
covid_speed$Country <- covid_speed$Country %>%
  replace(., countries=="Bosnia and Herzegovina", "Bosnia Herzegovina") %>%
  replace(., countries=="Czechia", "Czech Republic") %>%
  replace(., countries=="North Macedonia", "Macedonia")
covid_speed <- filter(covid_speed, Ratio != 0)
write.csv(covid_speed, "covid_speed.csv")

# plotting and saving the map, where each country is
# associated with its COVID-19 spread speed
europe <- st_read("geodata\\Europe\\Europe.shp")
europe <- europe[europe$NAME %in% covid_speed$Country, ]
europe$Ratio <- covid_speed$Ratio
ggplot(europe) +
  geom_sf(aes(fill = Ratio))+
  scale_fill_gradient(low='#FFCBBB', high='#FF2400') +
  ggtitle("COVID-19 spread speed")
ggsave("images\\covid_speed.png", scale = 1.1)

