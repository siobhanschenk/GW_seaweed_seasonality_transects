#### set up #####
library(tidyverse)
library(plyr)
library(readxl)
library(ggplot2);theme_set(theme(axis.text.x = element_text(face="bold"),
                                 axis.text.y = element_text(size=12, face="bold"),
                                 axis.title.x = element_text(size=15, face="bold"),
                                 axis.title.y = element_text(size=15, face="bold")))


a = read.csv("kelp_reproductive_timing.csv")
b = read_xlsx("GW_kelp_phenology_data.xlsx")


##### fix b #####
b$presabs = ifelse(b$visible_sori=="yes", "1", "0")

b = ddply(b, c("year", "month","day", "species"),
          summarise,
          nrepro = sum(as.numeric(presabs)))

#### fix a ####
a = a |> pivot_longer(
  cols=c(4:7),
  names_to = "species",
  values_to = "nrepro"
)


#### join a and b ####
both = full_join(a,b)

both$any_repro = ifelse(both$nrepro>0, "1","0")

both = both[,c(1:3,5,7)]

##### change species names #####
both$species = gsub("saccharina","saccharina_latissima", both$species)
both$species = gsub("nereo","nereocystis_luetkeana", both$species)
both$species = gsub("alaria","alaria_marginata", both$species)
both$species = gsub("costaria","costaria_costata", both$species)


##### save clean data #####
write.csv(both,"clean_reproductive_data.csv")
