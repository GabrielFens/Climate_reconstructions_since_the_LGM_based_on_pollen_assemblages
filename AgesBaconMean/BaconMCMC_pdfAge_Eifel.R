fass6=read.csv2("Eifel_MAT.csv",sep=';')
fass6=read.csv2("AssFEifel.csv",sep=';')
fass6=fass6[,-c(1)]
Age=read.csv2("ModeleAge_Eifel.csv",sep=';')
library(rioja)
library(vegan)
install.packages("riojaPlot", repos="https://nsj3.r-universe.dev")
remotes::install_github('nsj3/riojaPlot', build_vignettes=TRUE, dependencies=TRUE)
library(riojaPlot)

#Convertir abundance en %
a=rda(fass6)
Axis1=write.csv(a$CA$u[,1],'PCA_Eifel.csv')

fass6=fass6*100
colMax <- function(data){sapply(data, max, na.rm=TRUE)}
ma.sum=colMax(fass6)
ma.pollen1=fass6[, which(ma.sum > 15)]
p.col=c(rep("forestgreen",times=11),rep("gold2",times=3))

ma.pollen1=data.frame(ma.pollen1$Alnus,ma.pollen1$Betula,ma.pollen1$Carpinus,ma.pollen1$Corylus,ma.pollen1$Fagus,ma.pollen1$Fraxinus,ma.pollen1$Picea,ma.pollen1$Pinus,ma.pollen1$Quercus,ma.pollen1$Tilia,ma.pollen1$Ulmus,ma.pollen1$Cerealia.type,ma.pollen1$Artemisia,ma.pollen1$Poaceae)
colnames(ma.pollen1)=c("Alnus","Betula","Carpinus",'Corylus',"Fagus","Fraxinus","Picea","Pinus","Quercus","Tilia","Ulmus","Cerealia","Artemisia","Poaceae")
y.scale=seq(0,90,5)
ma.dist <- vegdist(ma.pollen1, method="bray", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = FALSE) 
ma.chclust <- chclust(ma.dist, method="coniss")
#ProfAge_LGP$X2/1E3


AgePlot=data.frame(Age)
colnames(AgePlot)=c("Age (ka cal BP)")
#ytks2=seq(0,20,1)
library(dplyr)
clust <- chclust(dist(sqrt(ma.pollen1)))
AgePlot$Zone <- cutree(clust, k=4)
zones <- AgePlot %>% group_by(Zone) %>% summarise(zm=mean.default(`Age (ka cal BP)`,na.rm = TRUE)) %>%
  mutate(name=paste("Zone", Zone)) %>% select(-Zone)
zone.names <- paste("Zone", 1:4)


pol.plot=riojaPlot(ma.pollen1,AgePlot,plot.sec.axis=TRUE,plot.exag=TRUE,ytks1=y.scale,srt.xlabel=60,y.rev=TRUE,plot.line=FALSE,plot.poly=TRUE,plot.bar=FALSE,col.poly.line="black",col.poly=p.col,scale.percent=TRUE,x.pc.omit0=TRUE,x.pc.inc=20,x.pc.omit0=TRUE)
addClustZone(pol.plot, ma.chclust, nZone=5, lwd=1, lty=2, col="black")

pol.plot=riojaPlot(ma.pollen1,AgePlot,sec.yvar.name="Depth (m)",plot.sec.axis=TRUE,plot.exag=TRUE,ytks1=y.scale,srt.xlabel=60,y.rev=TRUE,plot.line=FALSE,plot.poly=TRUE,plot.bar=FALSE,col.poly.line="black",col.poly=p.col,scale.percent=TRUE,x.pc.omit0=TRUE,x.pc.inc=20,x.pc.omit0=TRUE)
addClustZone(pol.plot, ma.chclust, nZone=5, lwd=1, lty=2, col="black")

#addRPZoneNames(zon,xRight=0.9, cex=0.6)  |>
# addRPClustZone(clust, col="red") |>
# addRPClust(clust)

library(scales)
d=data.frame(matrix(NA,ncol=2,nrow=nrow(Age)))
for (i in 1:nrow(Age))
{d[i,1]=length(which(ma.pollen1[1:11][i,]>1))
d[i,2]=length(which(ma.pollen1[12:14][i,]>1))
}
for(i in 1:(nrow(d)))
{d[i,]<-100*d[i,]/(sum(d[i,]))}
colnames(d)=c("Trees", "Herbs")

dform=data.frame(matrix(NA,ncol=1,nrow=2*nrow(Age)))
colnames(dform)="dform"
n=1
for (i in seq(1,nrow(dform),2))
{y=i+1
dform[i,1]=d[n,1]
dform[y,1]=d[n,2]
n=n+1
}
n=1
Age_sec=data.frame(matrix(NA,ncol=1,nrow=2*nrow(Age)))
colnames(Age_sec)="Age"

for (i in seq(1,nrow(Age_sec),2))
{y=i+1
Age_sec[i,1]=Age[n,1]
Age_sec[y,1]=Age[n,1]
n=n+1
}

dform$Age=Age_sec
dform$Group=c(rep(c("Trees","Herbs"),times=length(Age)))

ggplot(dform,aes(Age$Age,dform,fill=Group),label=Group,color=Group) +
  scale_fill_manual(values=c("gold2","forestgreen"))+
  geom_area()+
  theme_classic()
