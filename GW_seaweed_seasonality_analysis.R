##### IF YOU HAVEN'T INSTALLED PACKAGES RUN THIS, OTHERWISE SKIP IT #####
install.packages("tidyverse")
install.packages("plyr")
install.packages("ggplot2")


##### WORKSPACE SET UP #####

## load packages
library(tidyverse)
library(plyr)
library(ggplot2);theme_bw()

## tell R where to get the data. !!This is computer specific because it's where you downloaded the file
setwd("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/git_GW_seaweed_seasonality_transects")

## read in data
algae = read.csv("GW_seaweed_seasonality_transect_data.csv")

##### FORMAT DATA FOR ANALYSIS ####
## remove transects 1 and 5
algae = subset(algae, algae$transect_id!=1& algae$transect_id!=5)


## how many total columns -> makes it so you don't have to edit numbers as new algae are added
n=ncol(algae)

## make all algae abundance columns numeric 
algae[,7:n] <- sapply(algae[,c(7:n)], as.numeric)

## fill empty cells (instances of 0 percnet cover) with 0
algae[is.na(algae)]<-0


## pivot data. This is important for plotting and analysis later
algae.wide = algae %>% 
  pivot_longer(-c(1:6))

## rename value column
names(algae.wide)[names(algae.wide)=="value"]<-"percent_cover"

## separate out seaweed phylum info 
algae.wide = separate(data = algae.wide, 
                          col = name, 
                          into = c("seaweed_id","phylum"), 
                          sep = "__")

## summarize data across years for the same transect and month
algae.wide.grouped = ddply(algae.wide, c("transect_id","distance_along_transect_m","seaweed_id","phylum","month"), 
                       summarise,
                       N = length(percent_cover), ## sample size
                       mean = mean(percent_cover),
                       sd = sd(percent_cover)) %>%
  mutate(se = sd/sqrt(N), ## standard error
         lci = mean - 1.960*(se), ## lower bound of 95% confidence interval
         uci = mean + 1.960*(se))

## fill empty cells (instances of 0 percnet cover) with 0 (again)
algae.wide.grouped[is.na(algae.wide.grouped)]<-0

## set order of month
algae.wide.grouped$month = factor(algae.wide.grouped$month, levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))



##### BROWN SEAWEED PLOTS #####
## only keep brown seaweeds
brown = subset(algae.wide.grouped, algae.wide.grouped$phylum=="brown")


## heatmap of percent abundance by transect and month
ggplot(brown, aes(x=month, y=distance_along_transect_m, fill=mean))+
  geom_tile(color="white")+
  #geom_text(aes(label = mean), color = "grey50", size = 3)+
  scale_fill_continuous(
    low="white",high="goldenrod4")+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title.x = element_text(size=15, face="bold"),
        axis.title.y = element_text(size=15, face="bold"))+
  labs(x="Sampling Month", y="Distance From Seawall (m)", fill="Mean Percent Cover")+
  facet_grid(transect_id~seaweed_id, scales="free")+
  scale_y_reverse()


##### RED SEAWEED PLOTS #####
## only keep brown seaweeds
red = subset(algae.wide.grouped, algae.wide.grouped$phylum=="red")


## heatmap of percent abundance by transect and month
ggplot(red, aes(x=month, y=distance_along_transect_m, fill=mean))+
  geom_tile(color="white")+
  #geom_text(aes(label = mean), color = "grey50", size = 3)+
  scale_fill_continuous(
    low="white",high="red4")+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title.x = element_text(size=15, face="bold"),
        axis.title.y = element_text(size=15, face="bold"))+
  labs(x="Sampling Month", y="Distance From Seawall (m)", fill="Mean Percent Cover")+
  facet_grid(transect_id~seaweed_id, scales="free")+
  scale_y_reverse()

##### GREEN SEAWEED PLOTS #####
## only keep brown seaweeds
green = subset(algae.wide.grouped, algae.wide.grouped$phylum=="green")


## heatmap of percent abundance by transect and month
ggplot(green, aes(x=month, y=distance_along_transect_m, fill=mean))+
  geom_tile(color="white")+
  #geom_text(aes(label = mean), color = "grey50", size = 3)+
  scale_fill_continuous(
    low="white",high="green4")+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title.x = element_text(size=15, face="bold"),
        axis.title.y = element_text(size=15, face="bold"))+
  labs(x="Sampling Month", y="Distance From Seawall (m)", fill="Mean Percent Cover")+
  facet_grid(transect_id~seaweed_id, scales="free")+
  scale_y_reverse()

