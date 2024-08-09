library(rbacon)
library(rintcal)
library(ggplot2)
#Bacon_runs()DataLGP
#Bacon(ask=FALSE,coredir=tempfile())
#clam2bacon(DataLGP,bacondir = "Bacon_runs", clamdir = "clam_runs", sep = ";")
Bacon(core='/Users/gabrielfenisse/Documents/Thèse_PalynologieLGMEurope_1/ProgrammeGuiot/LGP_Age_MCM',prob=0.95)
#depths.file=DepthsLGP
Bacon(core="MSB2K",coredir='Bacon_runs/',prob=0.95)
Bacon(core='LDB',coredir = 'Bacon_runs/',d.min=0,thick=5,prob=0.95,unit ='cm',cc=1,cc1 = "IntCal20")

Depth_LDB<-read.csv2("Depth_LDB.csv",sep=';',dec=',')
Bacon.hist(Depth_LDB$depth)
agedepth(depths=Depth_LDB$depth[(Depth_LDB$depth>min(Data_LDB$depth))&(Depth_LDB$depth<max(Data_LDB$depth))],unit='cm',cc=0,title='LDB DataDepth',mn.lty=15,C14.col=rgb(0,1,1,0.95),mirror=TRUE)
#agedepth(unit='cm',d.by=200,title='Age estimated from LGP')
ProfAge_LDB=data.frame(matrix(NA,ncol=3,nrow=length(Depth_LDB))) 

for (i in 1:nrow(Depth_LDB))
{ages.First=Bacon.Age.d(Depth_LDB$depth[i])
ProfAge_LDB[i,1]=Depth_LDB$depth[i]
ProfAge_LDB[i,2]=mean(ages.First)
ProfAge_LDB[i,3]=sd(ages.First)
}

write.csv(ProfAge_LDB,"ModeleAge_LDB.txt")
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
g1<-ggplot(ProfAge_LDB,aes(x=X2/1E3))+
  stat_halfeye(adjust = 0.5,width=0.5,justification = -0.2,.width = 0, point_colour = NA)+
  theme_classic()
g2<-g1 +geom_boxplot(width = 0.12,outlier.color ="grey",alpha = 0.5) 
plot(g2)
plot(ProfAge_LDB$X2,ProfAge_LDB$X3,s='+')

ggplot(ProfAge_LDB)+
  geom_density(aes(x=X2), fill="green", color="darkgray",alpha = 0.7)+
  labs(title = "outputages", subtitle = "Probability density and mean")+
  theme_classic()

Baconvergence(core='LGP',runs=5,suggest=FALSE)

plot(ProfAge_LDB[,1],Depth_LDB$depth)
AgePredictedLDB<-read.csv2("LDB_163_ages.txt",sep="\t",dec='.')
plot(AgePredictedLDB$depth,AgePredictedLDB$mean)
N=data.frame(matrix(NA,ncol=4,nrow=nrow(ProfAge_LDB)))
for (i in 1:nrow(AgePredictedLDB))
{N[i,1]=AgePredictedLGP$depth[i]
N[i,2]=AgePredictedLGP$min[i]
N[i,3]=AgePredictedLGP$max[i]
N[i,4]=AgePredictedLGP$mean[i]
}
#PCA axes 1
a=rda(fass3)
Axis1=write.csv(a$CA$u[,1],'PCA_LDB.csv')

library(rioja)
library(vegan)
install.packages("riojaPlot", repos="https://nsj3.r-universe.dev")
remotes::install_github('nsj3/riojaPlot', build_vignettes=TRUE, dependencies=TRUE)
library(riojaPlot)
fass3<-read.csv2("AssFLDB.csv",sep=';',dec=',')
fass3=fass3[,-c(1)]

colMax <- function(data){sapply(data, max, na.rm=TRUE)}
ma.sum=colMax(fass3)
ma.pollen1=fass3[, which(ma.sum > 5)]
p.col=c(rep("forestgreen",times=16),rep("brown",times=1),rep("gold2",times=20))

ma.pollen1=data.frame(ma.pollen1$Abies,ma.pollen1$Alnus,ma.pollen1$Apiaceae,ma.pollen1$Asteraceae_subf._Cichorioideae,ma.pollen1$Asteraceae_subf._Asteroideae,ma.pollen1$Betula,ma.pollen1$Calluna_vulgaris,ma.pollen1$Corylus,ma.pollen1$Fagus,ma.pollen1$Fraxinus,ma.pollen1$Picea,ma.pollen1$Pinus,ma.pollen1$Quercus_robur.type,ma.pollen1$Rosaceae,ma.pollen1$Tilia,ma.pollen1$Ulmus,ma.pollen1$Juniperus,ma.pollen1$Chenopodiaceae.Amaranthaceae,ma.pollen1$Artemisia,ma.pollen1$Brassicaceae,ma.pollen1$Caryophyllaceae_.periporate_excl._Paronychioideae.,ma.pollen1$Cyperaceae,ma.pollen1$Helianthemum,ma.pollen1$Plantago,ma.pollen1$Poaceae,ma.pollen1$Polygonum_bistorta.type,ma.pollen1$Rumex.Oxyria,ma.pollen1$Thalictrum)
colnames(ma.pollen1)=c('Abies','Alnus','Apiaceae','Asteraceae Ligu','Asteraceae Tubu','Betula','Calluna vulgaris','Corylus',"Fagus",'Fraxinus','Picea',"Pinus","Quercus",'Rosaceae','Tilia','Ulmus','Juniperus',"Amaranthaceae","Artemisia",'Brassicaceae','Caryophyllaceae','Cyperaceae','Helianthemum','Plantago',"Poaceae",'Polygonum','Rumex','Thalictrum')
y.scale=seq(5,90,5)
ma.dist <- vegdist(ma.pollen1, method="bray", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = FALSE) 
ma.chclust <- chclust(ma.dist, method="coniss")
#ProfAge_LGP$X2/1E3

ma.pollen2=data.frame(fass3$Betula,fass3$Fraxinus,fass3$Picea)
colnames(ma.pollen2)=c('Betula','Fraxinus',"Picea")

AgePlot=data.frame(ProfAge_LDB$X2/1E3,ProfAge_LDB$X1/1E2)
colnames(AgePlot)=c("Age (ka cal BP)","Depth (m)")
#ytks2=seq(0,20,1)
library(dplyr)
clust <- chclust(dist(sqrt(ma.pollen1)))
AgePlot$Zone <- cutree(clust, k=4)
zones <- AgePlot %>% group_by(Zone) %>% summarise(zm=mean.default(`Age (ka cal BP)`,na.rm = TRUE)) %>%
  mutate(name=paste("Zone", Zone)) %>% select(-Zone)
zone.names <- paste("Zone", 1:4)

pol.plot=riojaPlot(ma.pollen1,AgePlot,sec.yvar.name="Depth (m)",plot.sec.axis=TRUE,plot.exag=TRUE,ytks1=y.scale,srt.xlabel=60,y.rev=TRUE,plot.line=FALSE,plot.poly=TRUE,plot.bar=FALSE,col.poly.line="black",col.poly=p.col,scale.percent=TRUE,x.pc.omit0=TRUE,x.pc.inc=20,x.pc.omit0=TRUE)
addClustZone(pol.plot, ma.chclust, nZone=5, lwd=1, lty=2, col="black")
pol.plot=riojaPlot(ma.pollen2,AgePlot,sec.yvar.name="Depth (m)",plot.sec.axis=TRUE,plot.exag=TRUE,ytks1=y.scale,srt.xlabel=60,y.rev=TRUE,plot.line=FALSE,plot.poly=TRUE,plot.bar=FALSE,col.poly.line="black",col.poly=p.col,scale.percent=TRUE,x.pc.omit0=TRUE,x.pc.inc=5,x.pc.omit0=TRUE)
addClustZone(pol.plot, ma.chclust, nZone=5, lwd=1, lty=2, col="black")

#addRPZoneNames(zon,xRight=0.9, cex=0.6)  |>
# addRPClustZone(clust, col="red") |>
# addRPClust(clust)

library(scales)
d=data.frame(matrix(NA,ncol=3,nrow=length(ProfAge_LDB$X2)))
for (i in 1:length(ProfAge_LDB$X2))
{d[i,1]=length(which(ma.pollen1[1:16][i,]>1))
d[i,2]=length(which(ma.pollen1[17][i,]>1))
d[i,3]=length(which(ma.pollen1[18:28][i,]>1))
}
for(i in 1:(nrow(d)))
{d[i,]<-100*d[i,]/(sum(d[i,]))}
colnames(d)=c("Trees", "Shrubs", "Herbs")

d$Age=ProfAge_LDB$X2

dform=data.frame(matrix(NA,ncol=1,nrow=3*length(ProfAge_LDB$X2)))
colnames(dform)="dform"
n=1
for (i in seq(1,nrow(dform),3))
{y=i+1
z=i+2
dform[i,1]=d[n,1]
dform[y,1]=d[n,2]
dform[z,1]=d[n,3]
n=n+1
}
n=1
Age=data.frame(matrix(NA,ncol=1,nrow=3*length(ProfAge_LDB$X2)))
colnames(Age)="Age"

for (i in seq(1,nrow(Age),3))
{y=i+1
z=i+2
Age[i,1]=ProfAge_LDB$X2[n]
Age[y,1]=ProfAge_LDB$X2[n]
Age[z,1]=ProfAge_LDB$X2[n]
n=n+1
}

dform$Age=Age
dform$Group=c(rep(c("Trees", "Shrubs", "Herbs"),times=length(ProfAge_LDB$X2)))

ggplot(dform,aes(Age$Age,dform,fill=Group),label=Group,color=Group) +
  scale_fill_manual(values=c("gold2","brown","forestgreen"))+
  geom_area()+
  theme_classic()
