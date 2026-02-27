###report for Great River Greening bumble bee surveys
library(dplyr)
library(vegan)
library(fossil)
library(pivottabler)
library(janitor)
library(viridis)
library(bipartite)
library(tidyr)


####all bumble bee data 2022 to 2025
bees=read.csv("cleaned_GRG-22-25.csv")

###Ritter data 2022 to 2025
ritterbees <- bees[ which(bees$site=='Ritter'), ]

#exclude rows with sp., N/A, or perching and abundances of 1
ritterbees2 <- ritterbees %>%
  filter(!Flowerspecies %in% c("sp.", "N/A", "perching"),
         !is.na(Flowerspecies),
         !FlowerGenus %in% c("perching"),
         !(Abundance <= 1))

####surveyeffort
effort=read.csv("cleanedGRGSamplingEffort22-25.csv")

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

##bee abundance over time
fm1=lm(totalbeesobserved ~ year, data = effort)
fm1
summary(fm1)
plot(fm1)

fm2=aov(sqrt(totalbeesobserved) ~ year, data = effort)
summary(fm2)
plot(fm2)

boxplot(effort$totalbeesobserved ~ effort$year,xlab="Year", ylab = "Average abundance per collection" )

# Add data points
#mylevels <- levels(as.factor(effort$totalbeesobserved))
#levels(as.factor(effort$LocationCode))
#levelProportions <- summary(effort$totalbeesobserved)/nrow(data)
#for(i in 1:length(mylevels)){
  
  #thislevel <- mylevels[i]
  #thisvalues <- data[data$names==thislevel, "value"]

##getting data into matrix of plant species, abundance. does not include bumble bees

beenetwork=ritterbees2
networktable=   beenetwork%>%
  group_by(FlowerName, BumbleBeeSpecies) %>%
  summarize(abundance = sum(Abundance))  

##going from long to wide
plotnetwork=networktable %>% 
  pivot_wider(., names_from = BumbleBeeSpecies, 
              values_from = abundance)

plotnetwork_mat <- as.matrix(plotnetwork[,-1])

#add plant names back into matrix

row.names(plotnetwork_mat) <- plotnetwork$FlowerName
head(plotnetwork_mat)

#remove that first row

plotnetwork_mat = plotnetwork_mat[-1,]

###making plot

##Transpose matrix so bees are rows and plants are columns
plotnetwork_mat_flipped <- t(plotnetwork_mat)

plotweb(plotnetwork_mat_flipped,
       method = "normal",
       text.rot = 90,
       col.low = "skyblue",     # left boxes (now bees)
       col.high = "orange",     # right boxes (now plants)
       col.interaction = "grey40",
       bor.col.interaction = "black") 



##bees per volunteer minute...box and whisker plots

effort <- effort %>%
  mutate(
    bees_per_vol_per_min = totalbeesobserved / (numberVolunteers * totalMinutes)
  ) #this column was already added in excel (see beespervolunteerperminute) but also wanted to make sure I could do it in r. 

library(ggplot2)

ggplot(effort, aes(x = factor(year), 
                   y = bees_per_vol_per_min, 
                   fill = factor(year))) +
  geom_boxplot() +
  scale_fill_manual(values = c("2022" = "#1b9e77",
                               "2023" = "#d95f02",
                               "2024" = "#7570b3",
                               "2025" = "#e7298a")) +
  theme_classic() +
  guides(fill = "none") +
  labs(
    x = "Year",
    y = "Bees per Volunteer per Minute"
  ) 
 
##bees per volunteer per hour....box and whisker plots

ggplot(effort, aes(x = factor(year), 
                   y = beespervolunteerperhour, 
                   fill = factor(year))) +
  geom_boxplot() +
  scale_fill_manual(values = c("2022" = "#1b9e77",
                               "2023" = "#d95f02",
                               "2024" = "#7570b3",
                               "2025" = "#e7298a")) +
  theme_classic() +
  guides(fill = "none") +
  labs(
    x = "Year",
    y = "Bees per Volunteer per Hour") +
  coord_cartesian(ylim = c(0,10))


##summarize bee abundance by plant
bees_by_plant <- ritterbees %>%
  group_by(FlowerName, BumbleBeeSpecies) %>%
  summarize(total_abundance = sum(Abundance), .groups = "drop")

#create pivot table of all bees observed on all plants 2022-2025
pt <- PivotTable$new()
pt$addData(ritterbees)
pt$addColumnDataGroups("BumbleBeeSpecies")
pt$addRowDataGroups("FlowerName")
pt$defineCalculation(calculationName="Abundance", summariseExpression="sum(Abundance)")
pt$renderPivot()

###plants visited by bees....graphs with relative abundance of bees on flowers. 
###heat color with bb species richness

                                              



###proportion of visits to native and non-native plants. compare ritter across years.



##interaction plots....use bipartite package



