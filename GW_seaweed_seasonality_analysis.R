## load packages
library(tidyverse)
library(ggplot2); theme_set(theme_bw()+
                              theme(panel.grid = element_blank(),
                                    strip.background = element_rect(fill="white"),
                                    axis.text = element_text(size = 12, color="black"), 
                                    axis.title = element_text(size=15, color="black")))

## set working directory
setwd("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/git_GW_seaweed_seasonality_transects")

## read in data
algae = read.csv("GW_seaweed_seasonality_transect_data.csv")


## dot-line plot (fucus)
ggplot(algae, aes(x=distance_along_transect_m, y=fucus_distichus_percent.cover, 
                  shape=sampling_date))+
  geom_point(cex=3)+
  geom_line()+
  facet_grid(transect_id~.)

## dot-line plot (ulva)
ggplot(algae, aes(x=distance_along_transect_m, y=ulva_fenestrata_percent.cover, 
                  shape=sampling_date))+
  geom_point(cex=3)+
  geom_line()+
  facet_grid(transect_id~.)

