##### SET UP ####
library(tidyverse)
library(plyr)
library(ggpubr)
library(ggplot2)+theme_set(theme_classic()+
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

setwd("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/git_GW_seaweed_seasonality_transects")

algae = read.csv("GW_seaweed_transects_data_cleaned.csv")
std = read.csv("stadiapole_datasheet.csv")
aug1 = read.csv("tide_obs_20230801.csv")
aug13 = read.csv("tide_obs_202308013.csv")



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
   scale_x_continuous(breaks = seq(0, 100, by = 5))+
  theme(legend.position = "none")
mintide
  
  
##### GET QUADRAT HEIGHTS #####
## keep extrapolated data to add back in later
ext = subset(std, std$height_measure=="extrapolated")
## keep real measures to match to tide heights
std = subset(std, std$height_measure=="real")


## fix bc tides bad formating
th = full_join(aug1, aug13)
th = th[,c(1,2)]
names(th) = c("datetime_pdt", "observations_m")

## sepaarte date and time PDT
th = separate(th, col=datetime_pdt,
                into = c("fulldate", "time_PDT"),
                sep=" ")

## separate date into year month day
th = separate(th, col=fulldate,
                into=c("year", "month", "day"),
                sep="-")

## fix colum format
th$year = as.numeric(th$year)
th$month = as.numeric(th$month)
th$day = as.numeric(th$day)


## join stadia pole data with tide observations
tideheights = inner_join(std, th)

## get correction factor for stadiapole height
tideheights$diff = ifelse(tideheights$correction=="vh", 1.58, 0)


## get corrected height of stadia pole
tideheights$corrected_stadia_height = tideheights$standiapole_height_m - as.numeric(tideheights$diff)

## get quadrat height
tideheights$quadrat_height = tideheights$corrected_stadia_height + tideheights$observations_m

## make a placeholder qudrat height variable for the extrapolated data
ext$quadrat_height = ext$extrapolated_quadrat_height_m

## add extrapolated heights back in
tideheights = full_join(ext, tideheights)

##### PLOT QUADRAT HEIGHT BY DISTANCE FROM SEAWEALL #####
## plot discrepancy between quadrat height and distance from seawall
height=ggplot(tideheights, aes(x=distance_from_seawall_m, y=quadrat_height, color=as.factor(transect_id)))+
  geom_jitter(cex=3)+
  scale_color_manual(values=c("yellowgreen","turquoise", "purple2"))+
  geom_smooth( se=F)+
  labs(x="Distance from the seawall (m)",
       y="Quadrat height (m)",
       color="Transect ID")+
 # xlim(0, 90)+
  #ylim(0,5)+
  scale_x_continuous(breaks = seq(0, 100, by = 5))
height


# Extract the legend. Returns a gtable
leg <- get_legend(height)
# Convert to a ggplot and print
leg=as_ggplot(leg)
# remove legend for real plot
height = height + theme(legend.position = "none")

##### ARRANGE PLOTS ######
ggarrange(mintide, height, leg, nrow=1,
          labels=c("A", "B", " "),
          widths = c(1, 1, 0.3))

ggsave("quadrat_height_by_distance.png", width=40, height = 15, units = "cm")

##### SAVE QUDRAT HEIGHT CSV #####
write.csv(tideheights, "tideheights_with_quadrat_height.csv")
