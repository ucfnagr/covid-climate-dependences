library(sf)
library(raster)
library(dplyr)
library(tidyverse)
library(fs)
library(rgdal)
library(ggplot2)
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
png("images\\world_temperatures_raster.png", res=300, width=2000, height=2000)
plot(temperature_world,
     col=colorRampPalette(c("#4B45F2", "#DDA0DD", "#FB3918"))(50),
     main="World temperatures, °??")
dev.off()

# temperature_europe is the previous raster, but it is masked with
# Europe countries I research
temperature_europe <- temperature_world %>%
  crop(., europe_borders) %>%
  mask(., europe_borders, na.rm=TRUE)
png("images\\europe_temperatures_raster.png", res=80, width=500, height=500)
plot(temperature_europe,
     col=colorRampPalette(c("#4B45F2", "#DDA0DD", "#FB3918"))(50),
     main="Temperature in Europe, °??")
dev.off()

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
png("images\\europe_humidity_raster.png", res=80, width=500, height=500)
plot(humidity_europe,
     col=colorRampPalette(c("#FFFFFF", "blue"))(50),
     main="Humidity in Europe, kPa")
dev.off()
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
png("images\\europe_windspeed_raster.png", res=80, width=500, height=500)
plot(windspeed_europe,
     col=colorRampPalette(c("#A1FA89", "#24A202"))(255),
     main="Wind speed in Europe, m/s")
dev.off()

# raster map, presenting the population density 
# it is also masked with Europe countries
density_world <- raster("geodata\\density.tif")
density_world <- raster::resample(density_world, temperature_world, method="ngb")
density_europe <- density_world %>%
  crop(., europe_borders) %>%
  mask(., europe_borders, na.rm=TRUE)
plot(density_europe)
png("images\\europe_density_raster.png", res=80, width=500, height=500)
plot(density_europe,
     col=colorRampPalette(c("white", "red"))(255),
     main="Population density in Europe")
dev.off()


# another data structure with Europe borders
europeDF <- readOGR("geodata\\Europe\\Europe.shp")

# here I recompute maps with the formula, presented in the research
# I put alpha = 0.3
temperature_europe <- 0.3 * temperature_europe + 0.7 * (temperature_europe * density_europe / 255)
humidity_europe <- 0.3 * humidity_europe + 0.7 * (humidity_europe * density_europe / 255) 
windspeed_europe <- 0.3 * windspeed_europe + 0.7 * (windspeed_europe * density_europe / 255) 

# calculating the average temperature for every Europe country
# fot this script, the tempuratures are averaged with respect 
# to population density
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
  ggtitle("Temperature in Europe countries in first four months of the year,\n averaged with respect to population density")
ggsave("images\\density_scaled_temperature.png", scale = 1.1)

# analogous to the previous block
ggplot(europe) +
  geom_sf(aes(fill = Wind))+
  scale_fill_gradient(low='yellow', high='red') +
  ggtitle("Wind speed in Europe countries in first four months of the year,\n averaged with respect to population density")
ggsave("images\\density_scaled_wind_speed.png", scale = 1.1)

# analogous to the previous blocks
ggplot(europe) +
  geom_sf(aes(fill = Vapor))+
  scale_fill_gradient(low='#00BFFF', high='blue') +
  ggtitle("Average water vapor pressure in Europe countries in first four\n months of the year, averaged with respect to population density")
ggsave("images\\density_scaled_water_vapor_pressure.png", scale = 1.1)

# writing the data I computed in .csv format
europe <- data.frame(europe$NAME, europe$Temp, europe$Vapor, europe$Wind)
names(europe) <- c("Country", "Temperature", "Humidity", "Windspeed")
write.csv(europe, "country_density_scaled.csv")

