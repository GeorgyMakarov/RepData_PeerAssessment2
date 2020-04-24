# Prerequisite libraries

library(reshape2)
library(dplyr)
library(lubridate)
library(ggplot2)

# Download data

raw_data_url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if (!file.exists("repdata_data_StormData.csv.bz2")) {
    download.file(url = raw_data_url, 
                  destfile = "repdata_data_StormData.csv.bz2")
    }

file_info <- file.info("repdata_data_StormData.csv.bz2")
storm_data <- read.csv("repdata_data_StormData.csv.bz2", 
                       sep = ",", header = TRUE)

stopifnot(file_info[,1] == 49177144)
stopifnot(dim(storm_data) == c(902297,37))

# Data processing

## Exploratory data analysis

names(storm_data)

## Subsetting the data

subset_data1 <- storm_data %>% 
    select(event_type = EVTYPE,
           fatalities = FATALITIES,
           injuries = INJURIES,
           property_damage = PROPDMG,
           property_multiplier = PROPDMGEXP,
           crop_damage = CROPDMG,
           crop_multiplier = CROPDMGEXP)

subset_data2 <- subset_data1 %>% 
    filter(fatalities >0 | injuries > 0 | 
               property_damage > 0 | crop_damage > 0 & event_type != "NONE")

dim(subset_data2)
sum(complete.cases(subset_data2))

## Cleaning typos

length(unique(subset_data2$event_type))


subset_data3 <- subset_data2 %>% 
    mutate(event_type = tolower(event_type))
length(unique(subset_data3$event_type))

subset_data3$event <- "other"
subset_data3$event[grep("avalanche", subset_data3$event_type)] <- "avalanche"
subset_data3$event[grep("blizzard", subset_data3$event_type)] <- "snow"
subset_data3$event[grep("flood", subset_data3$event_type)] <- "flood"
subset_data3$event[grep("wind", subset_data3$event_type)] <- "wind"
subset_data3$event[grep("fog", subset_data3$event_type)] <- "fog"
subset_data3$event[grep("cold", subset_data3$event_type)] <- "cold"
subset_data3$event[grep("chill", subset_data3$event_type)] <- "cold"
subset_data3$event[grep("frost", subset_data3$event_type)] <- "cold"
subset_data3$event[grep("freeze", subset_data3$event_type)] <- "cold"
subset_data3$event[grep("tornado", subset_data3$event_type)] <- "tornado"
subset_data3$event[grep("hail", subset_data3$event_type)] <- "hail"
subset_data3$event[grep("winds", subset_data3$event_type)] <- "wind"
subset_data3$event[grep("win", subset_data3$event_type)] <- "wind"
subset_data3$event[grep("wins", subset_data3$event_type)] <- "wind"
subset_data3$event[grep("storm", subset_data3$event_type)] <- "storm"
subset_data3$event[grep("rainstorm", subset_data3$event_type)] <- "storm"
subset_data3$event[grep("thunderstorm", subset_data3$event_type)] <- "storm"
subset_data3$event[grep("snow", subset_data3$event_type)] <- "snow"
subset_data3$event[grep("rain", subset_data3$event_type)] <- "rain"
subset_data3$event[grep("heat", subset_data3$event_type)] <- "heat"
subset_data3$event[grep("hurricane", subset_data3$event_type)] <- "hurricane"
subset_data3$event[grep("fld", subset_data3$event_type)] <- "flood"
subset_data3$event[grep("current", subset_data3$event_type)] <- "waves"
subset_data3$event[grep("surf", subset_data3$event_type)] <- "waves"
subset_data3$event[grep("fire", subset_data3$event_type)] <- "fire"
subset_data3$event[grep("water", subset_data3$event_type)] <- "flood"
subset_data3$event[grep("wave", subset_data3$event_type)] <- "waves"
subset_data3$event[grep("tsunami", subset_data3$event_type)] <- "waves"
subset_data3$event[grep("lightning", subset_data3$event_type)] <- "storm"
subset_data3$event[grep("warm", subset_data3$event_type)] <- "heat"
subset_data3$event[grep("torndao", subset_data3$event_type)] <- "tornado"
subset_data3$event[grep("high tides", subset_data3$event_type)] <- "waves"
subset_data3$event[grep("high seas", subset_data3$event_type)] <- "waves"
subset_data3$event[grep("marine mishap", subset_data3$event_type)] <- "waves"
subset_data3$event[grep("slide", subset_data3$event_type)] <- "slides"
subset_data3$event[grep("dust devil", subset_data3$event_type)] <- "heat"
subset_data3$event[grep("dry microburst", subset_data3$event_type)] <- "heat"
subset_data3$event[grep("low temperature", subset_data3$event_type)] <- "cold"
subset_data3$event[grep("freezing spray", subset_data3$event_type)] <- "cold"
subset_data3$event[grep("dam break", subset_data3$event_type)] <- "flood"


length(unique(subset_data3$event))

sum(sort(table(subset_data3$event), decreasing = TRUE)[1:10]) / nrow(subset_data3)

sum(sort(table(subset_data3$event), decreasing = TRUE)[1:10])
sort(table(subset_data3$event), decreasing = TRUE)[1:10]
subset_data4 <- subset_data3 %>% 
    filter(event %in% c("wind", "storm", "tornado", "flood", "hail", "snow", 
                        "fire", "rain", "heat", "waves"))
dim(subset_data4)

## Cleaning units

unique(subset_data4$property_multiplier)
unique(subset_data4$crop_multiplier)

subset_data5 <- subset_data4 %>% 
    mutate(property_multiplier = tolower(property_multiplier),
           crop_multiplier = tolower(crop_multiplier))

subset_data5$property_multiplier <- as.character(subset_data5$property_multiplier)
subset_data5$property_multiplier[is.na(subset_data5$property_multiplier)] <- 0
subset_data5$property_multiplier[!grepl("k|m|b|h|2|3|4|5|6|7", subset_data5$property_multiplier)] <- 0
subset_data5$property_multiplier[grep("k", subset_data5$property_multiplier)] <- "3"
subset_data5$property_multiplier[grep("m", subset_data5$property_multiplier)] <- "6"
subset_data5$property_multiplier[grep("b", subset_data5$property_multiplier)] <- "9"
subset_data5$property_multiplier[grep("h", subset_data5$property_multiplier)] <- "2"

subset_data5$crop_multiplier <- as.character(subset_data5$crop_multiplier)
subset_data5$crop_multiplier[is.na(subset_data5$crop_multiplier)] <- 0
subset_data5$crop_multiplier[!grepl("k|m|b|h|2|3|4|5|6|7", subset_data5$crop_multiplier)] <- 0
subset_data5$crop_multiplier[grep("k", subset_data5$crop_multiplier)] <- "3"
subset_data5$crop_multiplier[grep("m", subset_data5$crop_multiplier)] <- "6"
subset_data5$crop_multiplier[grep("b", subset_data5$crop_multiplier)] <- "9"
subset_data5$crop_multiplier[grep("h", subset_data5$crop_multiplier)] <- "2"

subset_data5$property_multiplier <- as.numeric(as.character(subset_data5$property_multiplier))
subset_data5$crop_multiplier <- as.numeric(as.character(subset_data5$crop_multiplier))

subset_data5 <- subset_data5 %>% 
    mutate(prop_damage = property_damage * 10^property_multiplier,
           crop_damage = crop_damage * 10^crop_multiplier) %>% 
    select(event, fatalities, injuries, crop_damage, prop_damage)

head(subset_data5)

## Data aggregation for health

health_dt <- subset_data5 %>% 
    group_by(event) %>% summarise(fatalities = sum(fatalities),
                                  injuries = sum(injuries))
fatal_h <- health_dt %>% 
    select(event, result = fatalities) %>% 
    mutate(type = "fatal")
injuries_h <- health_dt %>% 
    select(event, result = injuries) %>% 
    mutate(type = "injury")
health_dt <- rbind(fatal_h, injuries_h)
health_dt

## Data aggregation for economic

economic_dt <- subset_data5 %>% 
    group_by(event) %>% summarise(crop_damage_bln = sum(crop_damage) / 1000000000, 
                                  prop_damage_bln = sum(prop_damage) / 1000000000)
prop_econ <- economic_dt %>% 
    select(event, result = prop_damage_bln) %>% 
    mutate(type = "property damage")
crop_econ <- economic_dt %>% 
    select(event, result = crop_damage_bln) %>% 
    mutate(type = "crop damage")
economic_dt <- rbind(prop_econ, crop_econ)
economic_dt

# Which type of events are most harmful to health

health_dts <- health_dt %>% 
    group_by(event) %>% summarise(result = sum(result)) %>% 
    arrange(desc(result))
health_dts

ggplot(health_dt) +
    theme_bw() +
    ggtitle("Weather impact on population health") +
    xlab("Event type") + ylab("Fatalities and Injuries") +
    geom_col(aes(reorder(event, result), result, fill = type))

# Which types of events have the greatest economic consequences

economic_dts <- economic_dt %>% 
    group_by(event) %>% summarise(result = sum(result)) %>% 
    arrange(desc(result))
economic_dts

ggplot(economic_dt) +
    theme_bw() +
    ggtitle("Weather impact on economy") +
    xlab("Event type") + ylab("Loss in bln USD") +
    geom_col(aes(reorder(event, result), result, fill = type))

