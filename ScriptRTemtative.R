# Importing data & cleaning & converting
library(tidyverse)
library(ggmap)
library(ggplot2)
library(dplyr)
library(spData)
library(sf)
library(rgdal)
data(world)
US.sf <- world %>% dplyr::filter(iso_a2 == "US")
OTP <- read.csv("D:/UB/2018 Fall/503 R/geo503-2018-finalproject-jiyuanzh/data/OnTimeP.csv")
APdata <- read.csv("D:/UB/2018 Fall/503 R/geo503-2018-finalproject-jiyuanzh/data/AirportData.csv")
OTP.clean <- OTP %>% dplyr::filter(DEP_DELAY_NEW >= 1) %>% dplyr::filter(WEATHER_DELAY >= 1) %>% 
  dplyr::select(YEAR, MONTH, DAY_OF_MONTH, ORIGIN, ORIGIN_STATE_ABR, DEP_DELAY_NEW, WEATHER_DELAY)
OTP.sum <- OTP.clean %>% dplyr::select(ORIGIN, DEP_DELAY_NEW, WEATHER_DELAY) %>%
  group_by(ORIGIN) %>% summarize(total_delay = sum(DEP_DELAY_NEW), weather = sum(WEATHER_DELAY))
OTP.sum$ORIGIN <- sapply(OTP.sum$ORIGIN, as.character)
Airports <- APdata %>% select(AIRPORT, DISPLAY_AIRPORT_NAME, DISPLAY_AIRPORT_CITY_NAME_FULL, AIRPORT_COUNTRY_CODE_ISO, AIRPORT_STATE_CODE, LATITUDE, LONGITUDE)
A <- Airports %>% group_by(AIRPORT) %>% summarize(lat = mean(LATITUDE), lon = mean(LONGITUDE)) %>%
  dplyr::filter(lat != "NA") %>% dplyr::filter(lon != "NA")
A$AIRPORT <- sapply(A$AIRPORT, as.character)
Airport.sf <- st_as_sf(A, coords = c("lon", "lat"), crs = 4326)

#Inner join the On Time Performance data with Airport and make the first three plots
A_O.inner <- left_join(OTP.sum, A, by = c("ORIGIN" = "AIRPORT"))
A_O.inner.sf <- st_as_sf(A_O.inner, coords = c("lon", "lat"), crs = 4326)
p1 <- ggplot(US.sf) + geom_sf(aes(geometry = geom)) + geom_point(data = A_O.inner, aes(x = lon, y = lat))
p1
p2 <- ggplot(US.sf) + geom_sf(aes(geometry = geom)) + geom_point(data = A_O.inner, aes(x = lon, y = lat, size = weather))
p2
p3 <- ggplot(A_O.inner) + geom_point(aes(x = log(weather), y = log(total_delay))) + geom_smooth(method = loess, aes(x = log(weather), y = log(total_delay)))
p3
