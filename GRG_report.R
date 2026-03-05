###report for Great River Greening bumble bee surveys
library(dplyr)
library(vegan)
library(fossil)
library(pivottabler)
library(janitor)
library(viridis)
library(bipartite)
library(tidyr)
library(emmeans)


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
effort <- effort[ which(effort$LocationCode=='Ritter'), ]

###species matrix
rittermatrix <- create.matrix(ritterbees, tax.name = "BumbleBeeSpecies",
                    locality = "collectioncode",
                    abund.col = "Abundance",
                    abund = TRUE)


##species accumulation
sp1=specaccum(rittermatrix, method = "exact", permutations = 100,
          conditioned =TRUE, gamma = "jack1",  w = NULL)

plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
     xlab="Collection number", ylab = "Bumble bee richness +/- 95% C.I.", 
     main = "Species accumulation")

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

##Format relative abundance barplot
library(ggplot2)
library(RColorBrewer)
library(viridis)

##View only colorblind-friendly palettes
display.brewer.all(colorblindFriendly = TRUE)

ggplot(ritterbees, aes(x = factor(year), fill = BumbleBeeSpecies)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "PuOr") + #Using a colorblind friendly palette
  labs(title = "Bumble bee species abundance by year", 
       x = "Year",
       y = "Relative abundance",
       fill = "Bumble bee species") +
  theme(legend.text = element_text(face = "italic"))
  theme_minimal()


##bee abundance over time
fm1=lm(totalbeesobserved ~ year, data = effort)
fm1
summary(fm1)
plot(fm1)

#abundance square root transformed to meet assumptions of normality
fm2=aov(sqrt(totalbeesobserved) ~ year, data = effort)
summary(fm2)
plot(fm2)
boxplot(effort$totalbeesobserved ~ effort$year,xlab="Year", ylab = "Average abundance per collection +/- S.E." )

#still not normal. If we want to do this comparison need to use a non-parametric test


# Add data points
#mylevels <- levels(as.factor(effort$totalbeesobserved))
#levels(as.factor(effort$LocationCode))
#levelProportions <- summary(effort$totalbeesobserved)/nrow(data)
#for(i in 1:length(mylevels)){
  
  #thislevel <- mylevels[i]
  #thisvalues <- data[data$names==thislevel, "value"]

##getting data into matrix of plant species, abundance. does not include bumble bees



###Network
beenetwork=ritterbees2
networktable=   beenetwork%>%
  group_by(FlowerName, BumbleBeeSpecies) %>%
  summarize(abundance = sum(Abundance))  


##going from long to wide
plotnetwork=networktable %>% 
  pivot_wider(., names_from = BumbleBeeSpecies, 
              values_from = abundance)

plotnetwork_mat <- as.matrix(plotnetwork[,-1])
#replace NA with 0
plotnetwork_mat[is.na(plotnetwork_mat)] <- 0

#add plant names back into matrix

row.names(plotnetwork_mat) <- plotnetwork$FlowerName
head(plotnetwork_mat)

#remove that first row

plotnetwork_mat = plotnetwork_mat[-1,]

###making plot

##Transpose matrix so bees are rows and plants are columns
plotnetwork_mat_flipped <- t(plotnetwork_mat)

plotweb(plotnetwork_mat_flipped, horizontal = TRUE, 
        spacing = "auto", 
        higher_italic = TRUE, lower_italic = TRUE,
        text_size = 0.75, curved_links = TRUE)





##bees per volunteer minute...box and whisker plots with color

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
    y = "Bees per Volunteer per Hour", title="Bee capture rate",
  ) +
  theme(plot.title = element_text(hjust = 0.5))
 
##bees per volunteer per hour....box and whisker plots without color

ggplot(effort, aes(x = factor(year), 
                   y = beespervolunteerperhour, 
                   )) +
  geom_boxplot()  +
  theme_classic() +
  guides(fill = "none") +
  labs(
    x = "Year",
    y = "Bees per volunteer per hour", title="Bee capture rate",
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0,10))

##bee abundance over time
fm3=aov(beespervolunteerperhour ~ year, data = effort)
fm3
summary(fm3)
plot(fm3)

#transform to normalize
fm3=aov(log(beespervolunteerperhour) ~ year, data = effort)
fm3
summary(fm3)
plot(fm3)

##not normal...trying kruskal wallis
kruskal.test(beespervolunteerperhour ~ year, data = effort)

#comparing just 2022 and 2025
target <- c("2022", "2025")
effort2225=filter(effort, year %in% target)
fm4=kruskal.test(beespervolunteerperhour ~ year, data = effort2225)
fm4
summary(fm4)


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



