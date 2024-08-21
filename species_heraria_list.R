## set up #####
library(tidyverse)
library(plyr)

## herbaria
ulva <- read.csv("C:/Users/siobh/OneDrive/Desktop/ulva_beaty_herbarium_accession.csv")
v07 <- read_excel("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/git_GW_seaweed_seasonality_transects/UBC Herbarium Stanley Park transects 2024-07-16 representative vouchers.xlsx")
v08 <- read_excel("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/git_GW_seaweed_seasonality_transects/UBC Herbarium Stanley Park transects 2024-08-07 representative vouchers.xlsx")

## transect data
t <- read.csv("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/git_GW_seaweed_seasonality_transects/GW_seaweed_transects_data_cleaned.csv")


##### get species list #####
t = ddply(t, c("seaweed_id", "phylum"),
          summarise,
          n=length(percent_cover))
t = t[,c(2,1)]
t = separate(t, col="seaweed_id", into=c("Genus", "Species"), sep="_")


## format herbarium data  #####
ulva$Species = gsub("(sp)(\\.)", "sp", ulva$Species)
v07$Species = gsub("(sp)(\\.)", "sp", v07$Species)
v08$Species = gsub("(sp)(\\.)", "sp", v08$Species)

names(ulva)[names(ulva) == "Database.Field"] = "Collector Number"

## JOIN HERBARIUM DATA ####
v = full_join(v07, v08)
uv = full_join(ulva, v)

herbarium = uv[,c("Genus", "Species", "Collector Number", "Accession.Number")]

## format transect data #####
t$Genus = str_to_title(t$Genus)


## join herbaria and transect #####
splist = full_join(t, herbarium)

#write.csv(splist, "herbaria_species_list.csv")



##### make sp list wide ####
updatedsplist = read.csv("herbaria_species_list.csv")

spwide = pivot_wider(updatedsplist, names_from="Genus", values_from = "Species")
