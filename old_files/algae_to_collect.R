library(tidyverse)
library(plyr)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

herb = read.csv("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/herbaria_list_2024-07-12.csv")
algae = read.csv("C:/Users/siobh/OneDrive - The University Of British Columbia/Project - Seaweed Seasonality Transects/seaweed_seasonality_2021-09-05/git_GW_seaweed_seasonality_transects/GW_seaweed_transects_data_cleaned.csv")

algae = separate(algae, col="seaweed_id", into = c("genus", "species"), sep="_")
algae = ddply(algae, c("genus", "species"), summarise, n=length(genus))
algae = algae[,c("genus", "species")]
algae$genus = firstup(algae$genus)


herb = separate(herb, col="seaweed", into = c("genus", "species"), sep=" ")


both = full_join(herb, algae)


write.csv(both, "target_collections.csv")
getwd()
