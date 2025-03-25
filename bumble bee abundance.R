require(dplyr)
require(tidyr)
require(lubridate)
require(ggplot)
library(BiodiversityR) # also loads vegan
library(ggplot2)
library(ggsci)
library(readxl)
options(stringsAsFactors = FALSE)

collectiondata=read.csv("collectiondata.csv", header=TRUE)
bbdata22=read.csv("2022bumblebeedata.csv",header=TRUE)
bbdata23=read.csv("2023bumblebeedata.csv",header=TRUE)

head(collectiondata)
head(bbdata22)


df1=left_join(collectiondata,bbdata22,by="collectioncode")
head(df1)
str(df1)
write.table(df1, "G:/My Drive/R/GreatRiverGreening.txt", sep="\t")
write.table(df2, "G:/My Drive/Research/R/data/PhD/2genusfreq.csv")

df2=left_join(collectiondata,bbdata23,by="collectioncode")
write.table(df2, "G:/My Drive/R/GreatRiverGreening23.txt", sep="\t")

####checked and compiled in excel
effort=read.csv("effortGRG.csv", header=TRUE)
species=read.csv("speciesGRG.csv", header=TRUE)


################rarefy sp accumulation

require(BiodiversityR)
accumresult(species, permutations=100,
            conditioned=T, gamma="boot")

###bysite/year
speciesSL23=read.csv("springlake2023.csv", header=TRUE)
accumresult(speciesSL23, permutations=100,
            conditioned=T, gamma="boot")


speciesSL22=read.csv("springlake2022.csv", header=TRUE)
accumresult(speciesSL22, permutations=100,
            conditioned=T, gamma="boot")



###bysite
speciesR2223=read.csv("ritter2223.csv", header=TRUE)
R2223accum=accumresult(speciesR2223, permutations=100,
            conditioned=T, gamma="boot")
accumplot(R2223accum, addit=F, labels="", ggtitle="Ritter Farm Park", col=1, ci=2, pch=1, type="p", cex=1,
          xlab="Survey dates", ylab="Species richness", cex.lab=1, cex.axis=1)

speciesSL2223=read.csv("spring2223.csv", header=TRUE)
SL2223accum=accumresult(speciesSL2223, permutations=100,
                       conditioned=T, gamma="boot")
accumplot(SL2223accum, addit=F, labels="", col=1, ci=2, pch=1, type="p", cex=1,
          xlab="survey dates", ylab="species richness", cex.lab=1, cex.axis=1)


grg2223sp=read.csv("grgspeciestable2223.csv", header=TRUE)
grgenv=read.csv("grgenv.csv", header=TRUE)
Accum.1 <- accumcomp(grg2223sp, y=grgenv, factor='site', 
                     method='exact', permutations=100, conditioned=FALSE, plotit=FALSE)
accum.long1 <- accumcomp.long(Accum.1, ci=NA, label.freq=5)
head(accum.long1)
plotgg1 <- ggplot(data=accum.long1, aes(x = Sites, y = Richness, ymax = UPR, ymin = LWR)) + 
  scale_x_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(aes(colour=Grouping), size=2) +
  geom_ribbon(aes(colour=Grouping), alpha=0.2, show.legend=FALSE) + 
  scale_colour_npg() +
  labs(x = "Survey dates", y = "Species richness", colour = "Site")+theme_bw()
plotgg1





##########################Catch rate  Abundance
##boxplot of catch rate
catch=read.csv("GRGcatchrate.csv", header=TRUE)
catch$year<-as.factor(catch$year)

ritter <- catch[catch$site == "Ritter Farm", ]
ritter22 <- ritter[ritter$year == "2022", ]
ritter23 <- ritter[ritter$year == "2023", ]

spring <- catch[catch$site == "Spring Lake", ]
spring22 <- spring[spring$year == "2022", ]
spring23 <- spring[spring$year == "2023", ]

p1 <- ggplot(ritter, aes(x=year, y=catchrate)) + 
  geom_boxplot()+labs(x = "Year", y = "Ave. bees per person per hour")+ylim(0,7)+theme_bw()+
  geom_boxplot(fill="firebrick", alpha=0.8)


p2 <- ggplot(ritter23, aes(x=year, y=catchrate)) + 
  geom_boxplot()+
  labs(x = "Year", y = "Ave. bees per person per hour")+ylim(0,8)+theme_bw()+
  geom_boxplot(fill="firebrick", alpha=0.8)

p3 <- ggplot(spring, aes(x=year, y=catchrate)) + geom_boxplot()+
  labs(x = "Year", y = "Ave. bees per person per hour")+ylim(0,7)+
theme_bw()+geom_boxplot(fill="darkcyan", alpha=0.5)
  

p4 <- ggplot(spring23, aes(x=year, y=catchrate))+geom_boxplot()+
  labs(x = "Year", y = "Bees per person per hour")+ylim(0,8)+
theme_bw()  +geom_boxplot(fill="darkcyan", alpha=0.5)

p1
p2
p3
p4

####################t test
catch22 <- catch[catch$year == "2022", ]
catch23 <- catch[catch$year == "2023", ]

fm1=kruskal.test(catchrate ~ site, data = catch) 
fm1

fm3=kruskal.test(catchrate ~ year, data = catch) 
fm3

###2022
fm2=kruskal.test(catchrate ~ site, data = catch22)
fm2

