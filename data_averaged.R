library(sf)
library(raster)
library(dplyr)
library(tidyverse)
library(fs)
library(rgdal)

# europe_borders contains information about countries borders as
# a simple feature collection of MULTIPOLYGONs
europe <- st_read("geodata\\Europe\\Europe.shp")
europe_borders <- europe[0]

# temperature_world is a raster world map of tempertature
# averaged over first 4 months of 1970-2000 years
temperature_world <- dir_info("geodata\\temperature") %>%
  filter(str_detect(path, "0[1234].tif")) %>%
  select(path)%>%
  pull()%>%
  as.character()%>%
  stack() %>%
  mean()

# temperature_europe is the previous raster, but it is masked with
# Europe countries I research
temperature_europe <- temperature_world %>%
  crop(., europe_borders) %>%
  mask(., europe_borders, na.rm=TRUE)
plot(temperature_europe)

# humidity_world is analogous to the temperature_world
humidity_world <- dir_info("geodata\\humidity") %>%
  filter(str_detect(path, "0[1234].tif")) %>%
  select(path)%>%
  pull()%>%
  as.character()%>%
  stack() %>%
  mean()

# humidity_europe is analogous to the temperature_europe
humidity_europe <- humidity_world %>%
  crop(., europe_borders) %>%
  mask(., europe_borders, na.rm=TRUE)
plot(humidity_europe)

# windspeed_world is analogous to the temperature_world
windspeed_world <- dir_info("geodata\\windspeed") %>%
  filter(str_detect(path, "0[1234].tif")) %>%
  select(path)%>%
  pull()%>%
  as.character()%>%
  stack() %>%
  mean()
# windspeed_europe is analogous to the temperature_europe
windspeed_europe <- windspeed_world %>%
  crop(., europe_borders) %>%
  mask(., europe_borders, na.rm=TRUE)
plot(windspeed_europe)

# another data structure with Europe borders
europeDF <- readOGR("geodata\\Europe\\Europe.shp")

# calculating the average temperature for every Europe country
temperature_europe.vals <- raster::extract(temperature_europe, europeDF)
temperature_europe.means <- lapply(temperature_europe.vals, FUN=mean)
europe$Temp <- unlist(temperature_europe.means)

# analogous to the previous block
humidity_europe.vals <- raster::extract(humidity_europe, europeDF)
humidity_europe.means <- lapply(humidity_europe.vals, FUN=mean)
europe$Vapor <- unlist(humidity_europe.means)

# analogous to the previous blocks
windspeed_europe.vals <- raster::extract(windspeed_europe, europeDF)
windspeed_europe.means <- lapply(windspeed_europe.vals, FUN=mean)
europe$Wind <- unlist(windspeed_europe.means)

# plotting and saving the computed average temperature map
# colors are selected to match the palette of typical map 
ggplot(europe) +
  geom_sf(aes(fill = Temp))+
  scale_fill_gradient(low='#18ADFF', high='#FF9418') +
  ggtitle("Average temperature in Europe countries in first four months of the year, °C")
ggsave("images\\averaged_temperature.png", scale = 1.1)

# analogous to the previous block
ggplot(europe) +
  geom_sf(aes(fill = Wind))+
  scale_fill_gradient(low='yellow', high='red') +
  ggtitle("Average wind speed in Europe countries in first four months of the year, m/s")
ggsave("images\\averaged_wind_speed.png", scale = 1.1)

# analogous to the previous blocks
ggplot(europe) +
  geom_sf(aes(fill = Vapor))+
  scale_fill_gradient(low='#00BFFF', high='blue') +
  ggtitle("Average water vapor pressure in Europe countries in first four months of the year, kPa")
ggsave("images\\averaged_water_vapor_pressure.png", scale = 1.1)

# writing the data I computed in .csv format
europe <- data.frame(europe$NAME, europe$Temp, europe$Vapor, europe$Wind)
names(europe) <- c("Country", "Temperature", "Humidity", "Windspeed")
write.csv(europe, "country_averaged.csv")
