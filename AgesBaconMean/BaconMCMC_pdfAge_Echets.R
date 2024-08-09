library(rbacon)
library(rintcal)
library(ggplot2)
Bacon(core='/Users/gabrielfenisse/Documents/Thèse_PalynologieLGMEurope_1/ProgrammeGuiot/LGP_Age_MCM',prob=0.95)
#depths.file=DepthsLGP
Bacon(core="MSB2K",coredir='Bacon_runs/',prob=0.95)
Bacon(core='Echets',coredir = 'Bacon_runs/',thick=5,prob=0.95,unit ='cm',cc=1,cc1 = "IntCal20")

Depth_Echets<-read.csv2("Depth_Echets_Bacon.csv",sep=';',dec='.')
Depth_Echets=Depth_Echets*1E2
Bacon.hist(Depth_LGP$Depth)
agedepth(depths=Depth_Echets$depth[(Depth_Echets$depth>min(DataLGP$depth))&(Depth_Echets$depth<max(DaraLGP$depth))],unit='cm',cc=0,title='LGP DataDepth',mn.lty=15,C14.col=rgb(0,1,1,0.95),mirror=TRUE)
#agedepth(unit='cm',d.by=200,title='Age estimated from LGP')
ProfAge_Echets=data.frame(matrix(NA,ncol=3,nrow=length(Depth_Echets))) 
accurate.depth
for (i in 1:nrow(Depth_Echets))
{ages.First=Bacon.Age.d(Depth_Echets$Depth[i])
ProfAge_Echets[i,1]=Depth_Echets$Depth[i]
ProfAge_Echets[i,2]=mean(ages.First)
ProfAge_Echets[i,3]=sd(ages.First)
}
AccRates=data.frame(matrix(NA,ncol=4,nrow=length(Depth_LGP)))
for (i in 1:nrow(Depth_LGP))
{if (is.na(ProfAge_LGP$X2[i])==FALSE)
{ages.First=accrate.age(ProfAge_LGP$X2[i],cmyr=TRUE)
AccRates[i,1]=ProfAge_LGP$X2[i]
AccRates[i,2]=ProfAge_LGP$X3[i]
AccRates[i,3]=mean(ages.First,na.rm=TRUE)
AccRates[i,4]=sd(ages.First,na.rm=TRUE)
}}
ggplot(AccRates,aes(AccRates$X1/1E3,AccRates$X3))+
  #geom_point(xlim=c(0,50))+
  geom_line(xlim=c(0,50))+
  #geom_errorbar(aes(ymin=(AccRates$X3-AccRates$X4),ymax=(AccRates$X3+AccRates$X4)),alpha = 0.2)+
  theme_classic()

#add.dates(ProfAge_LGP$X2,ProfAge_LGP$X3,ProfAge_LGP$X1,cc=0)

library(ggplot2)
library(ggdist)
library(tidyverse)
g1<-ggplot(ProfAge_Echets,aes(x=X2/1E3))+
  stat_halfeye(adjust = 0.5,width=0.5,justification = -0.2,.width = 0, point_colour = NA)+
  theme_classic()
g2<-g1 +geom_boxplot(width = 0.12,outlier.color ="grey",alpha = 0.5) 
plot(g2)

ggplot(ProfAge_Echets)+
  geom_density(aes(x=X2), fill="green", color="darkgray",alpha = 0.7)+
  labs(title = "outputages", subtitle = "Probability density and mean")+
  theme_classic()

Baconvergence(core='LGP',runs=5,suggest=FALSE)

plot(ProfAge_Echets[,1],Depth_Echets$depth)
AgePredictedEchets<-read.csv2("Echets_106_ages.txt",sep="\t",dec='.')
plot(AgePredictedLGP$depth,AgePredictedLGP$mean)
N=data.frame(matrix(NA,ncol=4,nrow=nrow(ProfAge_Echets)))
for (i in 1:nrow(AgePredictedEchets))
{N[i,1]=AgePredictedLGP$depth[i]
N[i,2]=AgePredictedLGP$min[i]
N[i,3]=AgePredictedLGP$max[i]
N[i,4]=AgePredictedLGP$mean[i]
}
plot(ProfAge_LGP$X2,ProfAge_LGP$X3,s='+')
fass5=read.csv2("Echets_Fass4_EMPD.csv",sep=';')
fass5=read.csv2("AssFEchets.csv",sep=';')
fass5=fass5[,-c(1)]
NamesEchets=read.csv2("NamesEchets.txt",sep='\t')
colnames(fass5)=NamesEchets$Names

#Convertir abundance en %
a=rda(fass5)
Axis1=write.csv(a$CA$u[,1],'PCA_Echets.csv')

library(rioja)
library(vegan)
install.packages("riojaPlot", repos="https://nsj3.r-universe.dev")
remotes::install_github('nsj3/riojaPlot', build_vignettes=TRUE, dependencies=TRUE)
library(riojaPlot)

fass5=read.csv2("AssFEchets.csv",sep=";",dec=',')
NamesEchets=read.csv2("NameEchets.txt",sep=',')
fass5=fass5[,-c(1)]
colnames(fass5)=NamesEchets$Names

ProfAge_Echets=read.csv2("ModeleAge_Echets.csv",sep=',',dec='.')
Age_Echets=Age_Echets$X2
colMax <- function(data){sapply(data, max, na.rm=TRUE)}
ma.sum=colMax(fass5)
ma.pollen1=fass5[, which(ma.sum > 0.05)]
p.col=c(rep("forestgreen",times=6),rep("gold2",times=8))

ma.pollen1=data.frame(ma.pollen1$Betula,ma.pollen1$Carpinus,ma.pollen1$Corylus,ma.pollen1$Picea,ma.pollen1$Pinus,ma.pollen1$`Quercus robur-type`,ma.pollen1$`Chenopodiaceae/Amaranthaceae`,ma.pollen1$Artemisia,ma.pollen1$Cyperaceae,ma.pollen1$Poaceae,ma.pollen1$Helianthemum,ma.pollen1$Thalictrum)
colnames(ma.pollen1)=c("Betula","Carpinus","Corylus","Picea","Pinus","Quercus","Amaranthaceae","Artemisia","Cyperacées","Helianthemum","Poaceae","Thalictrum")
y.scale=seq(5,90,5)
ma.dist <- vegdist(ma.pollen1, method="bray", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = FALSE) 
ma.chclust <- chclust(ma.dist, method="coniss")
#ProfAge_LGP$X2/1E3


AgePlot=data.frame(ProfAge_Echets$X2/1E3,ProfAge_Echets$X1/1E2)
colnames(AgePlot)=c("Age (ka cal BP)","Depth (m)")
#ytks2=seq(0,20,1)
library(dplyr)
clust <- chclust(dist(sqrt(ma.pollen1)))
AgePlot$Zone <- cutree(clust, k=4)
zones <- AgePlot %>% group_by(Zone) %>% summarise(zm=mean.default(`Age (ka cal BP)`,na.rm = TRUE)) %>%
  mutate(name=paste("Zone", Zone)) %>% select(-Zone)
zone.names <- paste("Zone", 1:4)


pol.plot=riojaPlot(ma.pollen1*100,AgePlot,sec.yvar.name="Depth (m)",plot.sec.axis=TRUE,plot.exag=TRUE,ytks1=y.scale,srt.xlabel=60,y.rev=TRUE,plot.line=FALSE,plot.poly=TRUE,plot.bar=FALSE,col.poly.line="black",col.poly=p.col,scale.percent=TRUE,x.pc.omit0=TRUE,x.pc.inc=20,x.pc.omit0=TRUE)
addClustZone(pol.plot, ma.chclust, nZone=5, lwd=1, lty=2, col="black")
#addRPZoneNames(zon,xRight=0.9, cex=0.6)  |>
# addRPClustZone(clust, col="red") |>
# addRPClust(clust)

library(scales)
d=data.frame(matrix(NA,ncol=2,nrow=length(ProfAge_Echets$X2)))
ma.pollen1=ma.pollen1*100
for (i in 1:length(ProfAge_Echets$X2))
{d[i,1]=length(which(ma.pollen1[1:6][i,]>1))
d[i,2]=length(which(ma.pollen1[6:12][i,]>1))
}
for(i in 1:(nrow(d)))
{d[i,]<-100*d[i,]/(sum(d[i,]))}
colnames(d)=c("Trees", "Herbs")

d$Age=ProfAge_Echets$X2

dform=data.frame(matrix(NA,ncol=1,nrow=2*length(ProfAge_Echets$X2)))
colnames(dform)="dform"
n=1
for (i in seq(1,nrow(dform),2))
{y=i+1
dform[i,1]=d[n,1]
dform[y,1]=d[n,2]
n=n+1
}
n=1
Age=data.frame(matrix(NA,ncol=1,nrow=2*length(ProfAge_Echets$X2)))
colnames(Age)="Age"

for (i in seq(1,nrow(Age),2))
{y=i+1
Age[i,1]=ProfAge_Echets$X2[n]
Age[y,1]=ProfAge_Echets$X2[n]
n=n+1
}

dform$Age=Age
dform$Group=c(rep(c("Trees", "Herbs"),times=length(ProfAge_Echets$X2)))

ggplot(dform,aes(Age$Age,dform,fill=Group),label=Group,color=Group) +
  scale_fill_manual(values=c("gold2","forestgreen"))+
  geom_area()+
  theme_classic()

###Extra+
#pol.plot=riojaPlot(ma.pollen1,AgePlot/1E3,ymin=10,ymax=90,plot.sec.axis=FALSE,ytks1=y.scale,srt.xlabel=60,y.rev=TRUE, plot.line=FALSE, plot.poly=TRUE,plot.bar=FALSE,col.poly.line="black",col.poly=p.col,scale.percent=TRUE, xSpace=0.4, x.pc.inc=20, x.pc.omit0=TRUE)
#addClustZone(pol.plot, ma.chclust, nZone=5, lwd=1.5, lty=2, col="grey25")


