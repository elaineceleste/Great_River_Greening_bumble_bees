###report for Great River Greening bumble bee surveys
library(dplyr)
library(vegan)
library(fossil)
library(pivottabler)
library(janitor)
library(viridis)
library(bipartite)


####all bumble bee data 2022 to 2024
bees=read.csv("cleaned_GRGbumblebeedata.csv")

###Ritter data 2022 to 2024
ritterbees <- bees[ which(bees$site=='Ritter_Farm'), ]

####surveyeffort
effort=read.csv("samplingeffort.csv")

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

pt <- PivotTable$new()
pt$addData(ritterbees)
pt$addColumnDataGroups("year")
pt$addRowDataGroups("BumbleBeeSpecies")
pt$defineCalculation(calculationName="Abundance", summariseExpression="sum(Abundance)")
pt$renderPivot()


#### relative abundance

relabund=ritterbees %>%
  tabyl(year, BumbleBeeSpecies) %>%
  adorn_totals("col") %>% 
  adorn_percentages() 

 relabund=adorn_rounding(relabund, digits = 2, rounding = "half to even")

###transpose
transprelabund=relabund[-1] %>% t() %>% as.data.frame() %>% setNames(relabund[,1])

#####format table outside of R with relative abudance



##bees per volunteer hour...box and whisker plots




###plants visited by bees....graphs with relative abundance of bees on flowers. 
###heat color with bb species richness

                                              



###proportion of visits to native and non-native plants. compare ritter across years.



##interaction plots....use bipartite package



