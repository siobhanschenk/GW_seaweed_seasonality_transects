## Code by Siobhan Schenk

##### WORKSPACE SET UP ######
## load packages
library(shiny)
library(tidyverse)
library(plyr)
library(stringi)
library(data.table)
library(ggpubr)



## load data from Borealis https://doi.org/10.5683/SP3/IKGB6E
algae.wide = readRDS("Data/app_transect_data.RDS")
reproduction = readRDS("Data/app_repro_data.RDS")


##### MAKE ELEMENTS NEEDED FOR SERVER ######

## species list for dropdown selection
speclist = c(unique(algae.wide$seaweed_id))

#round values to 1 decimal place
algae.wide$quadrat_height_m=round(algae.wide$quadrat_height_m, digits = 1)

## replace NA wirh 0
algae.wide$percent_cover[is.na(algae.wide$percent_cover)]<-0

## group to plot mean by height
algae.wide.grouped = ddply(algae.wide, c("quadrat_height_m", "seaweed_id", "year","month","phylum"),
                           summarise,
                           mean = mean(percent_cover))

## use gsub to fix lables (need to separate because jan and Feb replace the 1 and 2 in Nov and Dec)
algae.wide.grouped$month <- stri_replace_all_regex(algae.wide.grouped$month,
                                                   pattern=c("3","4","5","6","7","8","9","10","11","12"),
                                                   replacement=c("Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."),
                                                   vectorize=FALSE)

algae.wide.grouped$month <- stri_replace_all_regex(algae.wide.grouped$month,
                                                   pattern=c("1","2"),
                                                   replacement=c("Jan.", "Feb."),
                                                   vectorize=FALSE)

## set order of month
algae.wide.grouped$month = factor(algae.wide.grouped$month, levels=c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."))


## use gsub to fix lables (need to separate because jan and Feb replace the 1 and 2 in Nov and Dec)
reproduction$month <- stri_replace_all_regex(reproduction$month,
                                             pattern=c("3","4","5","6","7","8","9","10","11","12"),
                                             replacement=c("Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."),
                                             vectorize=FALSE)

reproduction$month <- stri_replace_all_regex(reproduction$month,
                                             pattern=c("1","2"),
                                             replacement=c("Jan.", "Feb."),
                                             vectorize=FALSE)
## set order of month
reproduction$month = factor(reproduction$month, levels=c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."))


## rotate data long
repro.long = reproduction |> 
  pivot_longer(cols = c(4:7),
               names_to="seaweed_id",
               values_to = "repro_yn")
## convert to pres/abs
repro.long$repro_yn = ifelse(repro.long$repro_yn>0, "1", "0")

write_rds(algae.wide.grouped,"Data/app_transect_data_formatted.RDS")
write_rds(repro.long, "Data/app_repro_data_formatted.RDS")
