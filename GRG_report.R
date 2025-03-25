###report for Great River Greening bumble bee surveys
library(dplyr)


####all bumble bee data 2022 to 2024
bees=read.csv("G:/My Drive/R/GreatRiverGreening/greatrivergreeningbumblebees/cleaned_GRGbumblebeedata.csv")

###Ritter data 2022 to 2024
ritterbees <- bees[ which(bees$site=='Ritter_Farm'), ]

####surveyeffort
effort=read.csv("G:/My Drive/R/GreatRiverGreening/greatrivergreeningbumblebees/collectiondata.csv")

###table of bees by year

  group_by(ritterbees,year) %>%
  summarise(total = sum(Abundance), richness=count(BumbleBeeSpecies))

