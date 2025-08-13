##### SET UP #####
library(tidyverse)
library(ggpubr)
library(ggplot2);theme_set(theme_bw()+
                             theme(axis.text = element_text(size=12),
                                  axis.title = element_text(size=15)))

algae = read.csv("GW_seaweed_transects_data_cleaned.csv")


#### SUBSET DATA #####


## keep the good stuff
single_algae = subset(algae, 
                      algae$seaweed_id == "fucus_distichus" & ## only keep fucus
                       algae$distance_along_transect_m==0 ) ## only keep seawall quadrats


#### MAKE PLOT #####
p1=ggplot(single_algae, aes(x=as.numeric(month),
                         y=as.numeric(percent_cover)))+
  geom_point(cex=3, alpha=0.5, aes(shape=as.factor(transect_id), color=as.factor(year)))+
  geom_smooth(se=F, aes(color=as.factor(year)))+
  labs(x="month", y="Fucus % cover")
p1

  
p2=ggplot(single_algae, aes(x=as.numeric(month),
                           y=as.numeric(percent_cover),
                         color=as.factor(transect_id)))+
    geom_point(cex=1)+
    geom_smooth(se=F)+
   facet_grid(.~year, scales="free", space="free")+
  labs(x="month", y="Fucus % cover")
p2


ggarrange(p1, p2, ncol=1, nrow=2)
