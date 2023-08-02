##### SET UP ####
library(tidyverse)
library(plyr)
library(ggpubr)
library(ggplot2)+theme_set(theme_bw()+
                             theme(strip.background = element_rect(fill="white"),
                                   axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
                                   axis.text.x = element_text(colour = "black", face = "bold", size = 12),
                                   legend.text = element_text(size = 8, face ="bold", colour ="black"),
                                   legend.position = "right", axis.title.y = element_text(face = "bold", size = 14),
                                   axis.title.x = element_text(face = "bold", size = 14, colour = "black"),
                                   legend.title = element_text(size = 14, colour = "black", face = "bold"),
                                   legend.key=element_blank(),
                                   #axis.ticks = element_blank()
                             ))



algae = read.csv("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/git_GW_seaweed_seasonality_transects/GW_seaweed_transects_data_cleaned.csv")
std = read.csv("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/git_GW_seaweed_seasonality_transects/standpole_datasheet.csv")
aug1 = read.csv("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/git_GW_seaweed_seasonality_transects/tide_obs_20230801.csv")



###### SCATTERPLOT OF TIDE HEIGHT BY LOWEST QUADRAT NUMBER #####
## find lowest quadrat
lowest = ddply(algae, c("transect_id", "year", "month", "day", "low_tide_height_m"),
               summarise,
               furthest = max(distance_along_transect_m))

## plot
mintide=ggplot(lowest, aes(x=furthest, y=low_tide_height_m, color=as.factor(transect_id)))+
  geom_point(cex=3)+
  scale_color_manual(values=c("yellowgreen","turquoise", "purple2"))+
  geom_smooth(method='lm', se=F)+
  labs(x="Furthest quadrat from the seawall sampled (m)",
       y="Low tide height on sampling day (m)",
       color="Transect ID")+
   scale_x_continuous(breaks = seq(0, 100, by = 5))
mintide
  
  



##### GET QUADRAT HEIGHTS #####
## remove empty data
std = subset(std, std$standpole_height_m!="NA")

## fix bc tides bad formating
aug1 = aug1[,c(1,2)]
names(aug1) = c("datetime_pdt", "observations_m")

## sepaarte date and time PDT
aug1 = separate(aug1, col=datetime_pdt,
                into = c("fulldate", "time_PDT"),
                sep=" ")

## separate date into year month day
aug1 = separate(aug1, col=fulldate,
                into=c("year", "month", "day"),
                sep="-")

## fix colum format
aug1$year = as.numeric(aug1$year)
aug1$month = as.numeric(aug1$month)
aug1$day = as.numeric(aug1$day)


## join stadia pole data with tide observations
tideheights = inner_join(std, aug1)

## get correction factor for stadiapole height
  #!# need to be corrected to the actual height of the sight
tideheights$correction = "1.6256"

## get corrected height of stadia pole
tideheights$corrected_stadia_height = tideheights$standpole_height_m - as.numeric(tideheights$correction)

## get quadrat height
tideheights$quadrat_height = tideheights$corrected_stadia_height + tideheights$observations_m

## plot discrepancy between quadrat height and distance from seawall
height=ggplot(tideheights, aes(x=distance_from_seawall_m, y=quadrat_height, color=as.factor(transect_id)))+
  geom_point(cex=3)+
  scale_color_manual(values=c("yellowgreen","turquoise", "purple2"))+
  geom_smooth(method='lm', se=F)+
  labs(x="Distance from the seawall (m)",
       y="Quadrat height (m)",
       color="Transect ID")+
  xlim(0, 90)+
  scale_x_continuous(breaks = seq(0, 100, by = 5))

##### ARRANGE PLOTS ######
ggarrange(mintide, height, nrow=1,
          labels=c("A", "B"))

