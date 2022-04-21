## load packages
library(tidyverse)
library(plyr)
library(viridis)
library(ggplot2)+theme_set(theme_bw()+
                             theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
                                   axis.text.x = element_text(colour = "black", face = "bold", size = 12),
                                   legend.text = element_text(size = 12, face ="bold", colour ="black"),
                                   legend.position = "right", axis.title.y = element_text(face = "bold", size = 14),
                                   axis.title.x = element_text(face = "bold", size = 14, colour = "black"),
                                   legend.title = element_text(size = 14, colour = "black", face = "bold"),
                                   legend.key=element_blank()))

## set working directory
setwd("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/git_GW_seaweed_seasonality_transects")

## read in data
algae = read.csv("GW_seaweed_seasonality_transect_data1.csv")


## pivot data for analysis/graphing
algae.wide = algae %>% pivot_longer(-c(1:4))

## summarize data
algae.wide.grouped = ddply(algae.wide, c("transect_id","distance_along_transect_m","name"), 
                       summarise,
                       N = length(value), ## sample size
                       mean = mean(value),
                       sd = sd(value)) %>%
  mutate(se = sd/sqrt(N), ## standard error
         lci = mean - 1.960*(se), ## lower bound of 95% confidence interval
         uci = mean + 1.960*(se))


## plot all data
pd=position_dodge(0.1)
ggplot(data=algae.wide.grouped, aes(x=distance_along_transect_m, y=mean, colour=name))+
  geom_errorbar(aes(ymin=lci, ymax=uci), width=0.1, position=pd) +
  geom_point(position=pd)+
  geom_line(position=pd)+
  facet_grid(transect_id~.)+
  guides(colour=guide_legend(ncol=1))+
  ylab("mean percent cover")+
  xlab("distance from seawall (m)")
  
  