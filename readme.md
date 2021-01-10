# Is there a correlation between climatic conditions of European countries and the COVID-19 expansion speed?
The question of this study is whether there is a correlation between the climatic conditions of a country and the COVID-19 spread speed there.   

I analyze climate data in European countries, transform it with and without taking into account the population density map of a country, calculate the rate of spread of infection in different countries, and finally conduct a regression and test the hypothesis of a dependence between climate conditions and the spread speed.

## Research outline
- [Find data for the research](#Data-sources)
- [Prepare the climate data on Europe countires](#Europe-climate-data-preparation)
- [Compute the speed of COVID-19 spread](#COVID-19-spread-speed-computation)
- [Perform the regression and test the research hypothesis](#Regression-and-hypothesis-testing)
- [Results interpretation and discussion](#Results-interpretation-and-discussion)

# Data sources

[COVID spread data](https://github.com/datasets/covid-19) provides information on the number of reported infections, deaths, and recoveries in each country in the world. The dataset is maintained by the amazing team at Johns Hopkins University Center for Systems Science and Engineering.

[Europe countries boudaries](https://tapiquen-sig.jimdofree.com/english-version/free-downloads/europe/) dataset contains information about Europe countries boundaries =)

To analyze the climatic conditions I used raster maps from [Worldclim data](https://worldclim.org/data/worldclim21.html) for temperature, water vapor pressure and wind speed, with 10 min resolution. The dataset contains raster maps of one of the parameters (temperature / water vapor pressure / wind speed) for each month, averaged over 1970-2000 years.

[Population density raster map](https://neo.sci.gsfc.nasa.gov/view.php?datasetId=SEDAC_POP&date=2000-01-01) is taken from NASA sources.

All the used data can be found in this repository
#newline


# Europe climate data preparation
The code requires several packeges

```
library(sf)
library(raster)
library(dplyr)
library(tidyverse)
library(fs)
library(rgdal)
library(ggplot2)
```

First, I need to load boundaries of European countries. The shapefile contains several 
```
europe <- st_read("geodata\\Europe\\Europe.shp")
europe_borders <- europe[0]
```

After it, I prepare the raster map of average temperatures in Europe. As it is mentioned in the study, the map only requires data for first fourn months of the year. The loaded layers are stacked and averaged, so the result is the temperature, averaged over first 4 months
```
temperature_world <- dir_info("geodata\\temperature") %>%
  filter(str_detect(path, "0[1234].tif")) %>%
  select(path)%>%
  pull()%>%
  as.character()%>%
  stack() %>%
  mean()
```

The map looks the following way:
```
plot(temperature_world,
     col=colorRampPalette(c("#4B45F2", "#DDA0DD", "#FB3918"))(50),
     main="World temperatures, °С")
```
![Image alt](https://github.com/ucfnagr/covid-climate-dependences/blob/main/images/world_temperatures_raster.png)  

Then the map needs to be cropped to the size of Europe
```
temperature_europe <- temperature_world %>%
  crop(., europe_borders) %>%
  mask(., europe_borders, na.rm=TRUE)
```
Now you can see the cropped map, we can finally work with it
```
plot(temperature_europe,
     col=colorRampPalette(c("#4B45F2", "#DDA0DD", "#FB3918"))(50),
     main="Temperature in Europe, °С")
```
![Image alt](https://github.com/ucfnagr/covid-climate-dependences/blob/main/images/europe_temperatures_raster.png)   

Similarly, I load, stack, average and crop maps for humidity and wind speed. Let me skip that step and present the map now.  

![Image alt](https://github.com/ucfnagr/covid-climate-dependences/blob/main/images/europe_humidity_raster.png)   

![Image alt](https://github.com/ucfnagr/covid-climate-dependences/blob/main/images/europe_windspeed_raster.png)  

The research introduces to methods to average the climatic conditions data. The first method averages values directly. The second method takes into account the population density distribution in the country. The motivation for this decision is presented in the research paper. The second method requires to run the following two code blocks. If you are intrested only in the first method, you can skip these two blocks.

The processing of population density map is similar to the temperature map processing
```
density_world <- raster("geodata\\density.tif")
density_world <- raster::resample(density_world, temperature_world, method="ngb")
density_europe <- density_world %>%
  crop(., europe_borders) %>%
  mask(., europe_borders, na.rm=TRUE)
plot(density_europe,
     col=colorRampPalette(c("white", "red"))(255),
     main="Population density in Europe")
```
![Image alt](https://github.com/ucfnagr/covid-climate-dependences/blob/main/images/europe_density_raster.png)    

Climatic conditions map is updated according to the formula, presented in the research. Coefficient alpha is set to 0.3.
```
temperature_europe <- 0.3 * temperature_europe + 0.7 * (temperature_europe * density_europe / 255)
humidity_europe <- 0.3 * humidity_europe + 0.7 * (humidity_europe * density_europe / 255) 
windspeed_europe <- 0.3 * windspeed_europe + 0.7 * (windspeed_europe * density_europe / 255) 
```

This code block is similar for the temperature, wind speed and humidity. I overlay the borders of Europe on a raster image, and calculate the average parameter values within the country
```
temperature_europe.vals <- raster::extract(temperature_europe, europeDF)
temperature_europe.means <- lapply(temperature_europe.vals, FUN=mean)
europe$Temp <- unlist(temperature_europe.means)
```
Visualize the results (maps for humidity and windspeed are created the same way)
```
ggplot(europe) +
  geom_sf(aes(fill = Temp))+
  scale_fill_gradient(low='#18ADFF', high='#FF9418') +
  ggtitle("Temperature in Europe countries in first four months of the year,\n averaged with respect to population density")
```
![Image alt](https://github.com/ucfnagr/covid-climate-dependences/blob/main/images/density_scaled_temperature.png)    
![Image alt](https://github.com/ucfnagr/covid-climate-dependences/blob/main/images/density_scaled_water_vapor_pressure.png)    
![Image alt](https://github.com/ucfnagr/covid-climate-dependences/blob/main/images/density_scaled_wind_speed.png)    

# COVID-19 spread speed computation
The script requires the following libraries:
```
library(sf)
library(dplyr)
library(tidyverse)
library(fs)
library(rgdal)
```

First the script loads the data for the analysis
```
covidDF <- read.csv("covid.csv")
europe <- st_read("geodata\\Europe\\europe.shp")
countries <- europe$NAME
```
Several items in *countries* are actually belong to another country, so I exclude them. Several countries are named differently in Europe countries dataset and COVID-19 dataset, they need to be renamed
```
removing <- c("Gibraltar (UK)", "Guernsey (UK)",
              "Isle of Man (UK)", "Jersey (UK)",
              "Faeroe Islands (Denmark)", "Svalbard (Norway)",
              "Jan Mayen (Norway)")
countries <- countries[!countries %in% removing]
countries <- countries %>%
  replace(., countries=="Bosnia Herzegovina", "Bosnia and Herzegovina") %>%
  replace(., countries=="Czech Republic", "Czechia") %>%
  replace(., countries=="Macedonia", "North Macedonia")
```
The function which computes the speed of the COVID-19 spread takes the dataframe with fields *date*, *country* and *confirmed* cases. The function returns the single value - speed of the spread, which is actually the base of exponent function, determing the growth law.

I consider only dates, when the number of confirmed cases is greater than 100 (for smaller values  the function is hardly approximated by the exponent). I also consider only first four months as it is stated in the research

The detailed explanations on motivation for data filtering and exponent base computation formula are presented in the research.
```
compute_exp_base <- function(dataframe){
  dataframe <- dataframe %>%
    filter(., Confirmed > 100) %>%
    filter(., Date < "2020-05-01")
  conf <- dataframe$Confirmed
  # I exclude countries, which do not have enough
  # dates to approximate the spread speed
  if (length(conf) <= 5){
    return (0)
  } 

  base <- 0
  for (i in c(6:length(conf))){
    base <- base + sum(conf[(i-4):i]) / sum(conf[(i-5):(i-1)])
  }
  base <- base / (length(conf) - 5)
  return (base)
}
```

This part applies the presented function to the countries.
```
covid_speed <- data.frame(countries, rep(0, length(countries)))
names(covid_speed) <- c("Country", "Ratio")
counter <- 1
for (country in countries){
  covid_speed[counter, 2] <- compute_exp_base(filter(covidDF, Country==country))
  counter <- counter + 1
}
write.csv(covid_speed, "covid_speed.csv")
```

Let us see the results.
```
europe <- st_read("geodata\\Europe\\Europe.shp")
europe <- europe[europe$NAME %in% covid_speed$Country, ]
europe$Ratio <- covid_speed$Ratio
ggplot(europe) +
  geom_sf(aes(fill = Ratio))+
  scale_fill_gradient(low='#FFCBBB', high='#FF2400') +
  ggtitle("COVID-19 spread speed")
```
![Image alt](https://github.com/ucfnagr/covid-climate-dependences/blob/main/images/covid_speed.png)    

# Regression and hypothesis testing
Required libraries:
```
library(sf)
library(raster)
library(dplyr)
library(tidyverse)
library(fs)
library(rgdal)
```
Load the data on COVID-19 spread speed, computed in the previous section 
```
covid_speed <- read.csv("covid_speed.csv")
```

Perform regression on the data, generated by the first method
```
data <- read.csv("country_averaged.csv")
data <- data[data$Country %in% covid_speed$Country, ]
fit <- lm(covid_speed$Ratio ~ data$Temperature + data$Humidity + data$Windspeed)
print(summary(fit))
```
```
Call:
lm(formula = covid_speed$Ratio ~ data$Temperature + data$Humidity + 
    data$Windspeed)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.062205 -0.026975 -0.007098  0.025128  0.075131 

Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
(Intercept)       1.0193618  0.0650750  15.664   <2e-16 ***
data$Temperature -0.0054620  0.0055441  -0.985     0.33    
data$Humidity     0.1405113  0.1426169   0.985     0.33    
data$Windspeed   -0.0007573  0.0049789  -0.152     0.88    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.03589 on 41 degrees of freedom
Multiple R-squared:  0.026,	Adjusted R-squared:  -0.04526 
F-statistic: 0.3649 on 3 and 41 DF,  p-value: 0.7787
```
Perform regression on the data, generated by the second method
```
data <- read.csv("country_density_scaled.csv")
data <- data[data$Country %in% covid_speed$Country, ]
fit <- lm(covid_speed$Ratio ~ data$Temperature + data$Humidity + data$Windspeed)
print(summary(fit))
```
```
Call:
lm(formula = covid_speed$Ratio ~ data$Temperature + data$Humidity + 
    data$Windspeed)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.062205 -0.026975 -0.007098  0.025128  0.075131 

Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
(Intercept)       1.0193618  0.0650750  15.664   <2e-16 ***
data$Temperature -0.0054620  0.0055441  -0.985     0.33    
data$Humidity     0.1405113  0.1426169   0.985     0.33    
data$Windspeed   -0.0007573  0.0049789  -0.152     0.88    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.03589 on 41 degrees of freedom
Multiple R-squared:  0.026,	Adjusted R-squared:  -0.04526 
F-statistic: 0.3649 on 3 and 41 DF,  p-value: 0.7787
```


# Results interpretation and discussion

The regression results state confidently for all parameters that the hypothesis about the correlation between any of these parameters and the rate of spread of coronavirus should be rejected. 

Results for the temperature and humidity contradict to the most of the other studies on this topic, while results for the wind speed do not. Still, the topic is controversial, and there are other studies supporting the hypothesis of no correlation between the rate of spread of covid and the specified parameters. 

Finally, I assume that these parameters can indeed affect the rate of spread of the coronavirus, but such parameters are outside the parameters range, proposed in the study