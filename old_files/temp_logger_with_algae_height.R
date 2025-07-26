##### SET UP #####

## load packages
library(plyr)
library(tidyverse)
library(lubridate)
library(sandwich)
library(car)
library(plyr)
library(MASS)
library(nlme)
library(ggplot2)+theme_set(theme_bw()+
                             theme(panel.grid = element_blank(),
                                   strip.background = element_rect(fill="white"),
                                   axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
                                   axis.text.x = element_text(colour = "black", size = 8),
                                   legend.text = element_text(size = 8, face ="bold", colour ="black"),
                                   legend.position = "right", axis.title.y = element_text(face = "bold", size = 14),
                                   axis.title.x = element_text(face = "bold", size = 14, colour = "black"),
                                   legend.title = element_text(size = 14, colour = "black", face = "bold"),
                                   legend.key=element_blank()))

## tell R where to get the data
setwd("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - TempLoggers Data")
path = "C:/Users/siobh/OneDrive - The University Of British Columbia/Project - TempLoggers Data/SF_2022_temp_loggers_raw/"
  

## remove days up to and including deployment 
# high deployed on 2022-03-22
# low deployed on 2022-04-19

## read in data
temp_obs_high = read.csv("SF_2022_temp_loggers_raw/gw-04E8 0200 4022 09-20230604 130423.csv")
temp_obs_low = read.csv("SF_2022_temp_loggers_raw/gw-0407 B200 5E42 08-20220712 110552.csv")
#temps_early = read.csv("SF_2022_temp_loggers_raw/2022_temp_logger_data.csv")
logger_elevation <- read.csv("logger_elevation.csv")


##### FORMAT THE TEMPERATURE LOGGER DATA #####
## merge temps
temps = full_join(temp_obs_low, temp_obs_high)


## correct UTC by DAYLIHT SAVINGS TIME
## set variable format
temps$datetime.utc = as.POSIXct(temps$datetime.utc, format="%Y-%m-%d %H:%M")
temps$date.utc = substr(temps$datetime.utc, start = 1, stop = 10)

## separate date column
temps = separate(temps, col=date.utc, c("year", "month", "day"), sep="-")

##### SELECT 5 WARMEST OBSERVATIONS EACH DAY ######
## make groups
temps$daygroups = paste0(temps$site, temps$height, "-",temps$year, temps$month, temps$day)

## get list of groups
groups = c(unique(temps$daygroups))

## get temp extremes
## sort data by relative abundance
sorted = temps[order(-temps$temp),]

## make empty dataframe to store data
temp.df = NULL


## start loop
for(i in groups) {
  for(j in i) {
    ## subset dataframe by samples
    sample = subset(sorted, sorted$daygroup %in% c(j))
    
    ## get top 10 warmest temps
    top = sample[c(1:5),]
    
    ## get top 10 coldest temps
   # coldest = nrow(sample)
   # start.b5 = coldest - 4
    
   # bottom = sample[c(start.b5:coldest),]
    
    ## save list of top  abundance taxa
    t.tmp <- top
   # b.tmp <- bottom
    
    ## add top of bottom indicator
    t.tmp$place = "top5"
   # b.tmp$place = "bottom5"
    
    ## combine both top and bottom dfs
    #new.temps = merge(t.tmp, b.tmp, all=TRUE)
    
    ## add group indicator
    #new.temps$group = j
    
    ## bind with temporary dataframe
    temp.df <- rbind.fill(temp.df, t.tmp)
    
    ## close loop 
  }
}


##### PLOT THE TEMPS BY HEIGHT ######
temp.df$samp.date = as.Date(paste0(temp.df$year, "-", temp.df$month, "-", temp.df$day))

ggplot(temp.df, aes(x=samp.date, y=temp, color=height))+
  geom_smooth(linewidth=2, se=F)+
  scale_color_manual(values=c("#1E90FF","#000080"))

##### READ IN TRANSECT DATA #####
algae = read.csv("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/git_GW_seaweed_seasonality_transects/GW_seaweed_transects_data_cleaned.csv")

## get interesting subsets
interesting = subset(algae, algae$seaweed_id %in% 
                       c("ulva_sp","petalonia_fascia","saccharina_latissima","nereocystis_luetkeana",
                         "alaria_marginata","costaria_costata","laminariales_juvenile_recruits") &
                       algae$percent_cover>0)

## get max heights by smapling day
in.max = ddply(interesting, c("year", "month", "day", "seaweed_id"),
               summarise,
               max.height = max(quadrat_height_m))
## get date
in.max$samp.date = as.Date(paste0(in.max$year, "-", in.max$month, "-", in.max$day))

#### plot algae max height  ####
ggplot(in.max, aes(x=samp.date, y=max.height, color=seaweed_id))+
  geom_smooth(se=F, linewidth=2)+
  scale_color_manual(values=c("#D2B48C", "#BC8F8F", "#F4A460","#B8860B",
                              "#9932CC", "#D2691E", "#7CFC00"))

##### combine plots ######
temp.df$year = as.numeric(temp.df$year)
temp.df$month = as.numeric(temp.df$month)
temp.df$day = as.numeric(temp.df$day)

algaetemps = full_join(in.max, temp.df)

coeff = 0.1

ggplot(algaetemps, aes(x=samp.date))+
  geom_smooth(aes(y=temp, linetype=height), se=F)+
  geom_point(aes(y=max.height/coeff, color=seaweed_id), cex=3)+
  scale_y_continuous(name="Logger Temps",
                     sec.axis = sec_axis(~.*coeff, name="Algae max height"))+
  scale_color_manual(values=c("#D2B48C", "#BC8F8F", "#F4A460","#B8860B",
                              "#9932CC", "#D2691E", "#7CFC00", "white"))



