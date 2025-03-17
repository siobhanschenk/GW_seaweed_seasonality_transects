##### IF YOU HAVEN'T INSTALLED PACKAGES RUN THIS, OTHERWISE SKIP IT #####
#install.packages("tidyverse")
#install.packages("plyr")
#install.packages("readxl")


##### WORKSPACE SET UP #####

## load packages
library(tidyverse)
library(plyr)
library(readxl)
library(ggplot2);theme_set(theme(axis.text.x = element_text(face="bold"),
                                   axis.text.y = element_text(size=12, face="bold"),
                                   axis.title.x = element_text(size=15, face="bold"),
                                   axis.title.y = element_text(size=15, face="bold")))

## tell R where to get the data. !!This is computer specific because it's where you downloaded the file
#setwd("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/git_GW_seaweed_seasonality_transects")

## read in data
heights = read.csv("tideheights_with_quadrat_height.csv")
algae <- read_excel("GW_seaweed_seasonality_transect_data.xlsx")
algaecols = ncol(algae)


## create an invert datasets 
invertsub = algae[,c(1:7, 9:16)]

## only keep algae
algae = algae[,c(1:7, 17:algaecols)]


##### FORMAT DATA FOR ALGAL DATA FOR ANALYSIS ####
## remove transects 1 and 5
algae = subset(algae, algae$transect_id!=1& algae$transect_id!=5)


## how many total columns -> makes it so you don't have to edit numbers as new algae are added
n=ncol(algae)

## make all algae abundance columns numeric 
algae[,8:n] <- sapply(algae[,c(8:n)], as.numeric)

## fill empty cells (instances of 0 percnet cover) with 0
#algae[is.na(algae)]<-0

## pivot data. This is important for plotting and analysis later
algae.wide = algae %>% 
  pivot_longer(-c(1:7))

## rename value column
names(algae.wide)[names(algae.wide)=="value"]<-"percent_cover"

## separate out seaweed phylum info 
algae.wide = separate(data = algae.wide, 
                          col = name, 
                          into = c("seaweed_id","phylum"), 
                          sep = "__")

## make all the ulva_ be ulva_sp
algae.wide$seaweed_id=gsub("ulva_*", "ulvoid_sp__", algae.wide$seaweed_id)

## fix Ahnfeltia fastigiata
algae.wide$seaweed_id=gsub("ahnfeltia_sp", "ahnfeltia_fastigiata", algae.wide$seaweed_id)


## separate out old ulva species names
algae.wide = separate(data = algae.wide, 
                      col = seaweed_id, 
                      into = c("seaweed_id","old_ulva"), 
                      sep = "__")

## remove old ulva names 
algae.wide = algae.wide[,-c(9)]


## remove times where percent coover is 0
algae.wide.sub = subset(algae.wide, algae.wide$percent_cover >0)

algae.wide.sub$seaweed_id = gsub("ulva_sp", "ulvoid_sp", algae.wide.sub$seaweed_id)

## summarize data to get list of seaweeds only found once
algae.wide.grouped = ddply(algae.wide.sub, c("seaweed_id"), 
                       summarise,
                       occurence = length(percent_cover))

## get list of algae found only once
once = subset(algae.wide.grouped, algae.wide.grouped$occurence==1)
once = c(unique(once$seaweed_id))

print(once)


## replace "crusticorallina_sp"  in data with "crustose_coralline"
algae.wide$seaweed_id=gsub("crusticorallina_sp", "crustose_coralline", algae.wide$seaweed_id)
algae.wide$seaweed_id=gsub("lithothamnion_sp", "crustose_coralline", algae.wide$seaweed_id)

## remove unkonwn seaweed
algae.wide = subset(algae.wide, algae.wide$seaweed_id!="unknown")

##### REMOVE SEAWEEDS ONLY FOUND ONCE AND THAT DON'T MAKE SENSE FROM THE DATASET #####
algae.wide.ns = subset(algae.wide, !(algae.wide$seaweed_id %in% c("plocamium_sp")))

##### ADD THE QUADRAT HEIGHT BY STADIAPOLE ######
## format the heigths file
heights.sub = heights[,c(1,2,11)]
#names(heights) <- c("transect_id", "distance_along_transect_m", "quadrat_height_m")

## join with cleaned algal data
algae.heights = full_join(heights.sub, algae.wide.ns)

algae.heights = subset(algae.heights, algae.heights$seaweed_id !="X")


#### fix time colum ####
algae.heights = separate(algae.heights,
                         col = "low_tide_time_local_time",
                         into = c("trash", "low_tide_time_local_time"),
                         sep=" ")


algae.heights = algae.heights[,-c(7)]

##### save the cleaned file #####
write.csv(algae.heights, "GW_seaweed_transects_data_cleaned.csv", row.names=FALSE)
write.csv(invertsub, "GW_substrate_invert_data.csv", row.names=F)

write_rds(algae.heights, "seaweed_transects_gw/Data/app_transect_data.RDS")
repro <- read.csv("kelp_reproductive_timing.csv")
write_rds(repro, "seaweed_transects_gw/Data/app_repro_data.RDS")
