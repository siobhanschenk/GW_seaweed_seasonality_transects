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
setwd("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Freshet Saccharina/github_spring_freshet_saccharina_latissima/1-sf_imput_data")
path = "C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Freshet Saccharina/github_spring_freshet_saccharina_latissima/results_abiotic"
  
  
## read in data
tempdata =read.csv("2022_temp_logger_data.csv")
#tblow = read.csv("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Freshet Saccharina/spring_freshet_field_work_data_2022/SF_2022_temp_loggers_raw/tb_low_formatted.csv")
#tempdata = full_join(tempdata, tblow)

logger_elevation <- read.csv("logger_elevation.csv")
#fraser_discharge = https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/ ## this is from ferdous thesis
## tide observations files read in and merged
tide_obs <- list.files(path = "C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Freshet Saccharina/github_spring_freshet_saccharina_latissima/1-sf_imput_data/tide_observations",  # Identify all CSV files
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv, show_col_types = FALSE) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
tide_obs                                           # Print data to RStudio console



##### FORMAT THE TEMPERATURE LOGGER DATA #####
## remove bad data
tempdata = subset(tempdata, tempdata$keep=="yes" & tempdata$site %in% c("gw-high","gw-low"))

## set variable format
tempdata$datetime.utc = as.POSIXct(tempdata$datetime.utc, format="%Y-%m-%d %H:%M")
tempdata$datetime.pdt = as.POSIXct(tempdata$datetime.pdt, format="%Y-%m-%d %H:%M")
tempdata$temp = as.numeric(tempdata$temp)

## split columns
tempdata = separate(data = tempdata, 
                         col = site, 
                         into = c("site","position"), 
                         sep = "-")

## rename sites
site_id <- c(gw="GW", cp="CP", tb="TB", scp="SCP")
tempdata$site <- as.character(site_id[tempdata$site])

##### FORMAT THE TIDE DATA #####
## rename observations columns
names(tide_obs)[names(tide_obs)=="observations(m)"]<-"tide_height_m"

## split the dates column
tide_obs = tide_obs %>%
  separate(col=Date,
           sep=" ",
           into=c("date", "time", "timezone"))

## get date and time together to make datetime colums
tide_obs$datetime.pdt = paste(tide_obs$date,tide_obs$time)

## format datetime 
tide_obs$datetime.pdt = as.POSIXct(tide_obs$datetime.pdt, format="%Y-%m-%d %H:%M")


##### MERGE THE DATAFRAMES TOGETHER AND ADD UNDERWATER VARIABLE  ######

## merge
full_tide = full_join(tide_obs, tempdata)
                  
full_tide = full_join(full_tide, logger_elevation)

## remove ones with missing info
full_tide = subset(full_tide, full_tide$site=="GW" & full_tide$temp !="NA")


## add underwater variable
full_tide$underwater <- if_else(full_tide$elevation_m < full_tide$tide_height_m, "yes", "no")


##### SELECT THE TOP 5 WARMEST AND COLDEST MEASURE FOR EVERY POSITION AND DAY #####

## make groups
full_tide$daygroups = paste0(full_tide$site, full_tide$position, full_tide$date)

## get list of groups
groups = c(unique(full_tide$daygroups))

## get temp extremes
## sort data by relative abundance
sorted = full_tide[order(-full_tide$temp),]

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


##### calculate the mean +/- SD for each group
temps.grouped = ddply(temp.df, c("site","position","date", "place"), 
                        summarise,
                        N.temp = length(temp), ## sample size
                        mean.temp = mean(temp), ## mean
                        sd.temp = sd(temp))%>% ## standard deviation) %>%
  mutate(se.temp = sd.temp/sqrt(N.temp), ## standard error
         lci.temp = mean.temp - 1.960*(se.temp), ## lower bound of 95% confidence interval
         uci.temp = mean.temp + 1.960*(se.temp)) ## upper bound of 95% confidence interval


