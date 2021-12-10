## load packages
library(tidyverse)
library(plyr)
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


## group the data by aquariums and sampling date
algae_grouped = ddply(algae, c("sampling_date", "distance_along_transect_m"), 
                           summarise,
                           N.fd = length(fucus_distichus_percent.cover), ## sample size
                           mean.fd = mean(fucus_distichus_percent.cover), ## mean
                           sd.fd = sd(fucus_distichus_percent.cover)## standard deviation
                          )%>% ## standard deviation)
  mutate(se.fd = sd.fd/sqrt(N.fd), ## standard error
         lci.fd = mean.fd - 1.960*(se.fd), ## lower bound of 95% confidence interval
         uci.fd = mean.fd + 1.960*(se.fd)) ## upper bound of 95% confidence interval
 ## upper bound of 95% confidence interval



## dot-line plot (fucus)
ggplot(algae_grouped, aes(x=distance_along_transect_m, y=mean.fd, 
                  shape=sampling_date, color=sampling_date))+
  geom_point(cex=4)+
  geom_line()+
  geom_errorbar(aes(ymin=lci.fd, ymax=uci.fd), 
                width=0.5)+
  xlab("Distance from seawall (m)")+
  ylab("Fucus distichus % cover")+
  scale_color_manual(values=c("forestgreen","dodgerblue","cyan3"))

## dot-line plot (ulva)
ggplot(algae, aes(x=distance_along_transect_m, y=ulva_fenestrata_percent.cover, 
                  shape=sampling_date))+
  geom_point(cex=3)+
  geom_line()+
  facet_grid(transect_id~.)


## dot-line plot (mastocarpus)
ggplot(algae, aes(x=distance_along_transect_m, y=mastocarpus_sp_percent.cover, 
                  shape=sampling_date))+
  geom_point(cex=3)+
  geom_line()+
  facet_grid(transect_id~.)


## heatmap plot (fucus)
ggplot(algae, aes(x=distance_along_transect_m, y=fucus_distichus_percent.cover,
                  fill=fucus_distichus_percent.cover))+
  geom_tile()+
  facet_grid(transect_id~.)

