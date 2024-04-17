##### SET UP ######
library(readxl)
library(tidyverse)
library(plyr)
library(ggplot2)+theme_set(theme_classic()+
                             theme(strip.background = element_rect(fill="white"),
                                   axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
                                   axis.text.x = element_text(colour = "black", face = "bold", size = 12),
                                   legend.text = element_text(size = 12, face ="bold", colour ="black"),
                                   legend.position = "right", axis.title.y = element_text(face = "bold", size = 14),
                                   axis.title = element_text(face = "bold", size = 12, colour = "black"),
                                   legend.title = element_text(size = 12, colour = "black", face = "bold")))


## set filepath
#siobhan
setwd("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/git_GW_seaweed_seasonality_transects")

## read in file
raw_phenology = read_excel("GW_kelp_phenology_data.xlsx")

###### DATA FORMAT ######
raw_phenology$sampling_date = as.Date(paste0(raw_phenology$year,"-",raw_phenology$month,"-", raw_phenology$day))

sum_phenology = ddply(raw_phenology, c("species","sampling_date"),
                      summarise,
                      n.kelp = length(length_cm),  ## summary stats
                      mean.length = mean(length_cm),
                      mean.width = mean(blade_width_cm),
                      mean.NS = mean(nereo_stipe_length_cm),
                      mean.NW = mean(nereo_pneumatocyst_width_cm),
                      mean.mass = mean(mass_g),
                      sd.mass = sd(mass_g)) %>% 
 mutate(se.mass= sd.mass/sqrt(n.kelp),
                  error95.mass = 1.960*(se.mass)) ## 95 % confidence intervals 



##### BUBBLE GRAPHS #####

## all the kelps
ggplot(raw_phenology, aes(x=as.numeric(blade_width_cm), y=length_cm, color=visible_sori))+
  geom_point(alpha=0.7, cex=3)+
  scale_color_manual(values=c("#D98880","#6E2C00"))+
  facet_wrap(.~species, scales="free")

## all the kelps
ggplot(raw_phenology, aes(x=as.numeric(blade_width_cm), y=mass_g, 
                          color=visible_sori))+
  geom_point(alpha=0.7, cex=3)+
  scale_color_manual(values=c("#D98880","#6E2C00"))+
  facet_wrap(.~species, scales="free")


## nereo
nereo = subset(raw_phenology, raw_phenology$species=="nereo")

ggplot(nereo, aes(x=nereo_stipe_length_cm, y = nereo_pneumatocyst_width_cm, 
                  color=visible_sori))+
  geom_point(cex=3)


ggplot(nereo, aes(x=(nereo_stipe_length_cm), y=(length_cm), 
                  color=visible_sori))+
  geom_point(cex=3)

ggplot(nereo, aes(x=(length_cm), y = nereo_pneumatocyst_width_cm, 
                  color=visible_sori))+
  geom_point(cex=3)


##### LINE GRAPHS #####
