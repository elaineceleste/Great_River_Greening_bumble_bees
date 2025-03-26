###report for Great River Greening bumble bee surveys
library(dplyr)
library(vegan)
library(fossil)
library(gt)


####all bumble bee data 2022 to 2024
bees=read.csv("G:/My Drive/R/greatrivergreeningbumblebees/cleaned_GRGbumblebeedata.csv")

###Ritter data 2022 to 2024
ritterbees <- bees[ which(bees$site=='Ritter_Farm'), ]

####surveyeffort
effort=read.csv("G:/My Drive/R/greatrivergreeningbumblebees/collectiondata.csv")

###species matrix
rittermatrix <- create.matrix(ritterbees, tax.name = "BumbleBeeSpecies",
                    locality = "collectioncode",
                    abund.col = "Abundance",
                    abund = TRUE)

##species accululation
sp1=specaccum(rittermatrix, method = "exact", permutations = 100,
          conditioned =TRUE, gamma = "jack1",  w = NULL)

plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",xlab="collection", ylab = "richness")

#table by year

ritteryear <- create.matrix(ritterbees, tax.name = "BumbleBeeSpecies",
                              locality = "year",
                              abund.col = "Abundance",
                              abund = TRUE, )



