##### SET UP ######
library(readxl)
library(tidyverse)
library(plyr)
library(ggplot2)+theme_set(theme_bw()+
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
ggplot(raw_phenology, aes(x=blade_width_cm, y=length_cm, fill=species))+
  geom_point(pch=21, alpha=0.7, cex=3)+
  scale_fill_manual(values=c("#D98880","#6E2C00","#F4D03F","#B7950B"))+
 # scale_size(range=c(3,10))+
  guides(fill = guide_legend(override.aes = list(size = 5)))+
  geom_smooth(aes(color=species), method="lm", se=F)+
  scale_color_manual(values=c("#D98880","#6E2C00","#F4D03F","#B7950B"))

## all the kelps
ggplot(raw_phenology, aes(x=length_cm, y=mass_g, fill=species))+
  geom_point(pch=21, alpha=0.7, cex=3)+
  scale_fill_manual(values=c("#D98880","#6E2C00","#F4D03F","#B7950B"))+
  # scale_size(range=c(3,10))+
  guides(fill = guide_legend(override.aes = list(size = 5)))+
  geom_smooth(aes(color=species), method="lm", se=F, linewidth=2)+
  scale_color_manual(values=c("#D98880","#6E2C00","#F4D03F","#B7950B"))


## nereo
nereo = subset(raw_phenology, raw_phenology$species=="nereo")

ggplot(nereo, aes(x=nereo_stipe_length_cm, y = nereo_pneumatocyst_width_cm))+
  geom_point(pch=21, cex=3, aes(fill=as.factor(day)))+
  geom_smooth(se=F)


##### LINE GRAPHS #####
