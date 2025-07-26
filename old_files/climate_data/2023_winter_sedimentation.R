##### SET UP ######
library(tidyverse)
library(plyr)

setwd("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/climate_data")

## read in data
climate_data <- list.files(path = "C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/climate_data/pcds_data_2023_winter_sediment/EC_raw/",  # Identify all CSV files
                       pattern = "*.csv", full.names = TRUE) 
climate_data = lapply(climate_data, read_csv, show_col_types = FALSE) %>%  # Store all files in list
  set_names(climate_data) %>%  # keep the names of the .csv files
  bind_rows(.id='filename')  # Combine data sets into one data set with filename as new column
climate_data  # Print data to RStudio console


##### FORMAT DATA #####
climate_data$wind_speed = as.numeric(climate_data$wind_speed)

## remove NA wind observations
climate_data = subset(climate_data, climate_data$wind_speed >=0)


## delete file path
climate_data$filename = gsub("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/climate_data/pcds_data_2023_winter_sediment/EC_raw/", "", climate_data$filename)

## separate time column by day
climate_data = separate(climate_data, col = "time",
                        into =c("date", "obs_time"),
                        sep=" ")

#### remove dates that we don't want
climate_data$obs_date = climate_data$date

climate_data = separate(climate_data, col="obs_date",
                        into= c("year", "month", "day"),
                        sep="-")

climate_data$year = as.numeric(climate_data$year)

climate_data = subset(climate_data, climate_data$year>2022 )

#### SUBSET LOOP SET UP 
## make unique day and sation variable
climate_data$station_day = paste0(climate_data$filename, climate_data$date)

## list for loop 
sta.date = c(unique(climate_data$station_day))

## keep the 10 min and max observations for each station for each day 
## sort data by wind speed
sorted = climate_data[order(-climate_data$wind_speed),]

##### CALULCUATE WIND EXTREMES ######
## make empty dataframe to store output from the loop
wind_max = NULL
wind_min = NULL

## start loop
for(i in sta.date) {
  for(j in i) {
    
    ## subset dataframe by samples
    #!# Remeber to change te substrate to your group! 
    subsample = subset(sorted, sorted$station_day %in% c(j) & sorted$wind_speed >=0)
    
    ## get 10 max wind speed
    top = subsample[c(1:10),]
    
    ## save list of top wind speed
    t.tmp <- top
    wind_max <- rbind.fill(wind_max, t.tmp)
    
    ## get 10 min wind speed
    nobs = nrow(subsample)
    ten = nobs-9
    
    if(nobs<10){
      # low = the top dataframe to avoid errors
      low = top
      
      # easy subset out later #1
      low$keep = "no"
      
      # easy subset # 2
      low$ncol = nobs
      
      }else{
      
      # get min values
      low = subsample[c(ten:nobs),]
      
      # easy subset # 1
      low$keep = "yes"
      
      # easy subset # 2
      low$ncol = nobs
      
    }
    
    
    ## save list of top wind speed
    l.tmp <- low
    wind_min <- rbind.fill(wind_min, l.tmp)

    
    ## close loop 
  }
}

## add identifier for min and max observations
wind_max$group = "max"
wind_min$group = "min"

## subset min data
wind_min = subset(wind_min, wind_min$keep=="yes")

## check if there are any columns less than 20.
  # if there are, that means those observations are in the max dataset as well 
print(min(wind_min$ncol))


###### PLOT MAX DATA ######
ggplot(wind_max, aes(x=as.Date(date), 
                     y = as.numeric(wind_speed), 
                     color=filename))+
  #geom_smooth(se=F)+
  geom_line()+
  labs(y="Wind Speed (km/h)", x="Observation Date", color="Station ID")+
  theme_bw()

