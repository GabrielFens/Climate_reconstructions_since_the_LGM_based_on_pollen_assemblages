library(rbacon)
library(rintcal)
library(ggplot2)
DataLGP<-read.csv2("LGP3.csv",sep=',',dec='.')
#Bacon_runs()DataLGP
#Bacon(ask=FALSE,coredir=tempfile())
#clam2bacon(DataLGP,bacondir = "Bacon_runs", clamdir = "clam_runs", sep = ";")
Bacon(core='/Users/gabrielfenisse/Documents/Thèse_PalynologieLGMEurope_1/ProgrammeGuiot/LGP_Age_MCM',prob=0.95)
#depths.file=DepthsLGP
Bacon(core="MSB2K",coredir='Bacon_runs/',prob=0.95)
Bacon(core='LGP',coredir = 'Bacon_runs/',thick=5,prob=0.95,unit ='cm',cc=1,cc1 = "IntCal20")

Depth_LGP<-read.csv2("Depth_LGP.csv",sep=';',dec='.')
Bacon.hist(Depth_LGP$depth)
agedepth(depths=Depth_LGP$depth[(Depth_LGP$depth>min(DataLGP$depth))&(Depth_LGP$depth<max(DataLGP$depth))],unit='cm',cc=0,title='LGP DataDepth',mn.lty=15,C14.col=rgb(0,1,1,0.95),mirror=TRUE)
#agedepth(unit='cm',d.by=200,title='Age estimated from LGP')
ProfAge_LGP=data.frame(matrix(NA,ncol=3,nrow=length(Depth_LGP))) 

for (i in 1:nrow(Depth_LGP))
{ages.First=Bacon.Age.d(Depth_LGP$depth[i])
ProfAge_LGP[i,1]=Depth_LGP$depth[i]
ProfAge_LGP[i,2]=mean(ages.First)
ProfAge_LGP[i,3]=sd(ages.First)
}
write.csv(ProfAge_LGP,"ModeleAge_LGP.txt")

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
g1<-ggplot(ProfAge_LGP,aes(x=X2/1E3))+
  stat_halfeye(adjust = 0.5,width=0.5,justification = -0.2,.width = 0, point_colour = NA)+
  theme_classic()
g2<-g1 +geom_boxplot(width = 0.12,outlier.color ="grey",alpha = 0.5) 
plot(g2)

ggplot(ProfAge_LGP)+
  geom_density(aes(x=X2), fill="green", color="darkgray",alpha = 0.7)+
  labs(title = "outputages", subtitle = "Probability density and mean")+
  theme_classic()

Baconvergence(core='LGP',runs=5,suggest=FALSE)

plot(ProfAge_LGP[,1],Depth_LGP$depth)
AgePredictedLGP<-read.csv2("LGP_126_ages.txt",sep="\t",dec='.')
plot(AgePredictedLGP$depth,AgePredictedLGP$mean)
N=data.frame(matrix(NA,ncol=4,nrow=nrow(ProfAge_LGP)))
for (i in 1:nrow(AgePredictedLGP))
{N[i,1]=AgePredictedLGP$depth[i]
N[i,2]=AgePredictedLGP$min[i]
N[i,3]=AgePredictedLGP$max[i]
N[i,4]=AgePredictedLGP$mean[i]
}
plot(ProfAge_LGP$X2,ProfAge_LGP$X1)
fass4<-read.csv2("AssFLGP.csv",sep=';',dec=',')
fass4=fass4[,-c(1)]
NamePol=read.csv2("NameLGP.txt",sep='\t',dec=',')
names(fass4)<-NamePol$Name
#fass4=subset(fass4, select=-c(Myriophyllum))
#fass4=data.frame(fass4)

library(rioja)
library(vegan)
install.packages("riojaPlot", repos="https://nsj3.r-universe.dev")
remotes::install_github('nsj3/riojaPlot', build_vignettes=TRUE, dependencies=TRUE)
library(riojaPlot)

#Convertir abundance en %
a=rda(fass4)
Axis1=write.csv(a$CA$u[,1],'PCA_LGP.csv')

colMax <- function(data){sapply(data, max, na.rm=TRUE)}
ma.sum=colMax(fass4)
ma.pollen1=fass4[, which(ma.sum > 15)]
p.col=c(rep("forestgreen",times=12),rep("brown",times=1),rep("gold2",times=5))

ma.pollen1=data.frame(ma.pollen1$Alnus,ma.pollen1$Abies,ma.pollen1$Betula,ma.pollen1$Carpinus,ma.pollen1$Corylus,ma.pollen1$Fagus,ma.pollen1$Fraxinus,ma.pollen1$`Quercus_ilex.type`,ma.pollen1$Tilia,ma.pollen1$Ulmus,ma.pollen1$Picea,ma.pollen1$Pinus,ma.pollen1$Juniperus,ma.pollen1$Artemisia,ma.pollen1$Cyperaceae,ma.pollen1$Helianthemum,ma.pollen1$Poaceae,ma.pollen1$`Ranunculus.type`)
colnames(ma.pollen1)=c("Alnus","Abies","Betula","Carpinus",'Corylus',"Fagus","Fraxinus","Quercus","Tilia","Ulmus","Picea","Pinus","Juniperus","Artemisia","Cyperaceae","Helianthemum","Poaceae","Ranunculus")
y.scale=seq(5,90,5)
ma.dist <- vegdist(ma.pollen1, method="bray", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = FALSE) 
ma.chclust <- chclust(ma.dist, method="coniss")
#ProfAge_LGP$X2/1E3


AgePlot=data.frame(ProfAge_LGP$X2/1E3,ProfAge_LGP$X1/1E2)
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
#addRPZoneNames(zon,xRight=0.9, cex=0.6)  |>
 # addRPClustZone(clust, col="red") |>
 # addRPClust(clust)

library(scales)
d=data.frame(matrix(NA,ncol=3,nrow=length(ProfAge_LGP$X2)))
for (i in 1:length(ProfAge_LGP$X2))
{d[i,1]=length(which(ma.pollen1[1:12][i,]>1))
d[i,2]=length(which(ma.pollen1[13][i,]>1))
d[i,3]=length(which(ma.pollen1[14:18][i,]>1))
}
for(i in 1:(nrow(d)))
{d[i,]<-100*d[i,]/(sum(d[i,]))}
colnames(d)=c("Trees", "Shrubs", "Herbs")

d$Age=ProfAge_LGP$X2

dform=data.frame(matrix(NA,ncol=1,nrow=3*length(ProfAge_LGP$X2)))
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
Age=data.frame(matrix(NA,ncol=1,nrow=3*length(ProfAge_LGP$X2)))
colnames(Age)="Age"

for (i in seq(1,nrow(Age),3))
{y=i+1
z=i+2
Age[i,1]=ProfAge_LGP$X2[n]
Age[y,1]=ProfAge_LGP$X2[n]
Age[z,1]=ProfAge_LGP$X2[n]
n=n+1
}

dform$Age=Age
dform$Group=c(rep(c("Trees", "Shrubs", "Herbs"),times=length(ProfAge_LGP$X2)))

ggplot(dform,aes(Age$Age,dform,fill=Group),label=Group,color=Group) +
  scale_fill_manual(values=c("gold2","brown","forestgreen"))+
  geom_area()+
  theme_classic()

###Extra+
#pol.plot=riojaPlot(ma.pollen1,AgePlot/1E3,ymin=10,ymax=90,plot.sec.axis=FALSE,ytks1=y.scale,srt.xlabel=60,y.rev=TRUE, plot.line=FALSE, plot.poly=TRUE,plot.bar=FALSE,col.poly.line="black",col.poly=p.col,scale.percent=TRUE, xSpace=0.4, x.pc.inc=20, x.pc.omit0=TRUE)
#addClustZone(pol.plot, ma.chclust, nZone=5, lwd=1.5, lty=2, col="grey25")





DataTogether=data.frame(ProfAge_LGP,N)
ggplot(DataTogether,aes(X1,X2))+
  geom_point(color='black')+
  geom_point(aes(X1.1,X3.1),color='red')+
  theme_classic()

agedepth()
age.pMC(-2000,20,1)
Bacon.hist(AgeLGP$depth)

AgeLGP<-read.csv2("LGP_126_ages.txt",sep="\t",dec='.')
DataTogether=merge.data.frame(as.matrix(AgeLGP),as.matrix(DataLGP))
ggplot(AgePredictedLGP,aes(median,depth))+
    geom_line(color='red')+
    geom_line(aes(min,depth),color='green')+
    geom_line(aes(max,depth),color='blue')+
    geom_point(aes(DataLGP$age,DataLGP$depth),color='grey')+
    theme_classic()
DataLGP<-data.frame(DataLGP)
plot(as.matrix(DataLGP$age),as.matrix(DataLGP$depth))

set.initvals <- function(set=get('info'), core=set$core, values=c(), click=1) {
  elbows <- set$elbows
  
  if(length(values) == 0) { # then the user will click on an existing graph to select age-depth points
    message("Please select your initval age-depth points on the graph by clicking with your left mouse button. When you are done selecting, use right-click.")
    if(click == 1) # user right-clicks once done
      draft.agedepth <- locator() else
        draft.agedepth <- locator(click)
      points(draft.agedepth, col=2, pch=4, cex=5, lwd=2)
      message("Lovely.")
      ages <- approx(draft.agedepth$x, draft.agedepth$y, elbows, rule=2)$y
      agedepth <- cbind(elbows, ages)
      accs <- diff(ages) / diff(elbows)
      accs <- c(accs[1], accs)
      accs1 <- abs(jitter(accs)) # add some scatter to ensure that th
      accs2 <- abs(jitter(accs))
      ages1 <- jitter(ages[1])
      ages2 <- jitter(ages[2])
      w1 <- jitter(0.5) # memory
      w2 <- jitter(0.5)
      init1 <- c(ages1, accs1, w1)
      init2 <- c(ages2, accs2, w2)
  } else { # user provides 2 sets of starting points
    init1 <- values[1,]
    init2 <- values[2,]
  }
  write.table(rbind(init1, init2), paste0(set$bacon.file, ".init"), sep="\t", col.names=FALSE, row.names=FALSE)
}
