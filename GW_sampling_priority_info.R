##### SET UP #####
library(tidyverse)
library(plyr)
library(ggplot2);theme_set(theme_bw()+
                             theme(axis.text = element_text(size=12),
                                   axis.title = element_text(size=15)))

algae = read.csv("GW_seaweed_transects_data_cleaned.csv")

## remove abscence
algae = subset(algae, algae$percent_cover>0)


#### divide data ###
algae_all_transects = ddply(algae, c("seaweed_id", "transect_id"),
                            summarise,
                            placeholder = length(month))

## see how many algae are only in one transect
transect_count = ddply(algae_all_transects, c("seaweed_id"),
                        summarise,
                        ntransects = length(transect_id))

single_transect = subset(transect_count, transect_count$ntransects==1)


#### FIND WHERE THE SINGLES ARE #####
single_locations = subset(algae, algae$seaweed_id %in% c(single_transect$seaweed_id))

## generate summary table
single_locations_table = ddply(single_locations, c("seaweed_id","transect_id"),
                               summarise,
                               instance_number = length(seaweed_id),
                               max_height = max(quadrat_height_m),
                               min_height = min(quadrat_height_m),
                               earliest_month = min(month),
                               latest_month = max(month))
