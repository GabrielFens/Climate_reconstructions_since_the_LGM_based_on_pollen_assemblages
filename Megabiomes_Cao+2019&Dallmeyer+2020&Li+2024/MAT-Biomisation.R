################################################################################
#############################  Set up environment  #############################
################################################################################
######################  LOOCV same as rioja (Table S5.2)  ######################
################################################################################
#Biomisation-MAT
dir.create(here::here("training/loocv_as_rioja"), FALSE)
setwd(here::here("training/loocv_as_rioja"))

#Please, install these packages if you want to process it!
library(rioja)
library(analogue)

##################################  Training  for each fossil site ##################################
#Pollen records=load data
AssCalLGP=read.csv2("AssCalLGP.csv",sep=";",dec=',')
AssFLGP=read.csv2("AssFLGP.csv",sep=";",dec=',')
rownames(AssCalLGP)=AssCalLGP$X
AssCalLGP=AssCalLGP[,-c(1)]
AssCalLGP=AssCalLGP[which(row.names(AssCalLGP) %in% SiteClimAvailable),] 
rownames(AssFLGP)=AssFLGP$X
AssFLGP=AssFLGP[,-c(1)]

AssCalLDB=read.csv2("AssCalLDB.csv",sep=";",dec=',')
AssFLDB=read.csv2("AssFLDB.csv",sep=";",dec=',')
rownames(AssCalLDB)=AssCalLDB$X
AssCalLDB=AssCalLDB[,-c(1)]
AssCalLDB=AssCalLDB[which(row.names(AssCalLDB) %in% SiteClimAvailable),] 
rownames(AssFLDB)=AssFLDB$X
AssFLDB=AssFLDB[,-c(1)]

AssCalEchets=read.csv2("AssCalEchets.csv",sep=";",dec=',')
AssFEchets=read.csv2("AssFEchets.csv",sep=";",dec=',')
rownames(AssCalEchets)=AssCalEchets$X
AssCalEchets=AssCalEchets[,-c(1)]
AssCalEchets=AssCalEchets[which(row.names(AssCalEchets) %in% SiteClimAvailable),] 
rownames(AssFEchets)=AssFEchets$X
AssFEchets=AssFEchets[,-c(1)]

AssCalEifel=read.csv2("AssCalEifel.csv",sep=";",dec=',')
AssFEifel=read.csv2("AssFEifel.csv",sep=";",dec=',')
rownames(AssCalEifel)=AssCalEifel$X
AssCalEifel=AssCalEifel[,-c(1)]
AssCalEifel=AssCalEifel[which(row.names(AssCalEifel) %in% SiteClimAvailable),] 
rownames(AssFEifel)=AssFEifel$X
AssFEifel=AssFEifel[,-c(1)]

AssCalFuramoos=read.csv2("AssCalFuramoos.csv",sep=";",dec=',')
AssFFuramoos=read.csv2("AssFFuramoos_v3.csv",sep=";",dec=',')
rownames(AssCalFuramoos)=AssCalFuramoos$X
AssCalFuramoos=AssCalFuramoos[,-c(1)]
AssCalFuramoos=AssCalFuramoos[which(row.names(AssCalFuramoos) %in% SiteClimAvailable),] 
rownames(AssFFuramoos)=make.names(AssFFuramoos$X,unique=TRUE)
AssFFuramoos=AssFFuramoos[,-c(1)]

#Pollen datasets adapted to actual assemblages and measured climate  
#AssCal=AssCal[which(row.names(AssCal) %in% SiteClimAvailable),] 
SitePolAvailable=unique(rownames(AssCal)[AssCal!='NaN'])
VarClim=data.frame(VarClim)
rownames(VarClim)=rownames(AssCal)
AssCal=AssCal[which(row.names(AssCal) %in% SitePolAvailable),] 
VarClim=VarClim[which(row.names(VarClim) %in% SitePolAvailable),] 
VarClim=as.numeric(unlist(VarClim))
Samples=Samples[which(Samples$SiteName %in% SitePolAvailable),] 

SitePolAvailable=unique(rownames(AssCalLGP)[AssCalLGP!='NaN'])
VarClim=data.frame(VarClim)
rownames(VarClim)=rownames(AssCalLGP)
AssCalLGP=AssCalLGP[which(row.names(AssCalLGP) %in% SitePolAvailable),] 
VarClim=VarClim[which(row.names(VarClim) %in% SitePolAvailable),] 
VarClim=as.numeric(unlist(VarClim))

Age_LGP=read.csv2("ModeleAge_LGP.txt",sep=',',dec='.')
Age_LGP_2=read.csv2("ModeleAge_LGP2.txt",sep=',',dec='.')
Age_LDB=read.csv2("ModeleAge_LDB.txt",sep=',',dec='.')
Age_Furamoos=read.csv2("ModeleAge_Furamoos.csv",sep=',',dec='.')
Age_Echets=read.csv2("ModeleAge_Echets.txt",sep=',',dec='.')
Age_Eifel=read.csv2("ModeleAge_Eifel.csv")

#Dissimilarity coefficient/Compare training set dissimilarity
#Normal distribution
Analog=analog(AssCal,AssF,method="SQchord")
summary(Analog)
n=cma(Analog,cutoff=0.5)
plot(Age,log(n$n.analogs),type='l')
dij=dissim(Analog)
plot(dij)

################################################################################
################################ Reconstruction ################################
################################################################################
#MAT transfer function reconstruction - SQ chord
#CLass1
biome=rep(c("CLDE","TAIG","PION","CLMX","COCO","TEDE","COMX","WAMX","XERO","TUND","COST","WAST","CODE","HODE"),each=5)
biomeSelect=c("./LDB/CLDE.csv","./LDB/TAIG.csv","./LDB/PION.csv","./LDB/CLMX.csv","./LDB/COCO.csv","./LDB/TEDE.csv","./LDB/COMX.csv","./LDB/WAMX.csv","./LDB/XERO.csv","./LDB/TUND.csv","./LDB/COST.csv","./LDB/WAST.csv","./LDB/CODE.csv","./LDB/HODE.csv")
biomeSelect=c("./LGP/CLDE.csv","./LGP/TAIG.csv","./LGP/PION.csv","./LGP/CLMX.csv","./LGP/COCO.csv","./LGP/TEDE.csv","./LGP/COMX.csv","./LGP/WAMX.csv","./LGP/XERO.csv","./LGP/TUND.csv","./LGP/COST.csv","./LGP/WAST.csv","./LGP/CODE.csv","./LGP/HODE.csv")
biomeSelect=c("./Eifel/CLDE.csv","./Eifel/TAIG.csv","./Eifel/PION.csv","./Eifel/CLMX.csv","./Eifel/COCO.csv","./Eifel/TEDE.csv","./Eifel/COMX.csv","./Eifel/WAMX.csv","./Eifel/XERO.csv","./Eifel/TUND.csv","./Eifel/COST.csv","./Eifel/WAST.csv","./Eifel/CODE.csv","./Eifel/HODE.csv")
biomeSelect=c("./Echets/CLDE.csv","./Echets/TAIG.csv","./Echets/PION.csv","./Echets/CLMX.csv","./Echets/COCO.csv","./Echets/TEDE.csv","./Echets/COMX.csv","./Echets/WAMX.csv","./Echets/XERO.csv","./Echets/TUND.csv","./Echets/COST.csv","./Echets/WAST.csv","./Echets/CODE.csv","./Echets/HODE.csv")
biomeSelect=c("./Furamoos/CLDE.csv","./Furamoos/TAIG.csv","./Furamoos/PION.csv","./Furamoos/CLMX.csv","./Furamoos/COCO.csv","./Furamoos/TEDE.csv","./Furamoos/COMX.csv","./Furamoos/WAMX.csv","./Furamoos/XERO.csv","./Furamoos/TUND.csv","./Furamoos/COST.csv","./Furamoos/WAST.csv","./Furamoos/CODE.csv","./Furamoos/HODE.csv")
VariableClim=rep(c(1:5),times=length(unique(biome)))

#Class2
biome=rep(c("TAIG","COCO","TEDE","DRYT","CUSH","DWAR","DESE","COMX","PROS","SHRU","STEP","CLDE"),each=5)
biomeSelect=c("./LDB_2/TAIG.csv","./LDB_2/COCO.csv","./LDB_2/TEDE.csv","./LDB_2/DRYT.csv","./LDB_2/CUSH.csv","./LDB_2/DWAR.csv","./LDB_2/DESE.csv","./LDB_2/COMX.csv","./LDB_2/PROS.csv","./LDB_2/STEP.csv","./LDB_2/CLDE.csv")
biomeSelect=c("./LGP_2/TAIG.csv","./LGP_2/COCO.csv","./LGP_2/TEDE.csv","./LGP_2/DRYT.csv","./LGP_2/CUSH.csv","./LGP_2/DWAR.csv","./LGP_2/DESE.csv","./LGP_2/COMX.csv","./LGP_2/PROS.csv","./LGP_2/STEP.csv","./LGP_2/CLDE.csv")
biomeSelect=c("./Eifel_2/TAIG.csv","./Eifel_2/COCO.csv","./Eifel_2/TEDE.csv","./Eifel_2/DRYT.csv","./Eifel_2/CUSH.csv","./Eifel_2/DWAR.csv","./Eifel_2/DESE.csv","./Eifel_2/COMX.csv","./Eifel_2/PROS.csv","./Eifel_2/STEP.csv","./Eifel_2/CLDE.csv")
biomeSelect=c("./Furamoos_2/TAIG.csv","./Furamoos_2/COCO.csv","./Furamoos_2/TEDE.csv","./Furamoos_2/DRYT.csv","./Furamoos_2/CUSH.csv","./Furamoos_2/DWAR.csv","./Furamoos_2/DESE.csv","./Furamoos_2/COMX.csv","./Furamoos_2/PROS.csv","./Furamoos_2/STEP.csv","./Furamoos_2/CLDE.csv")
VariableClim=rep(c(1:5),times=length(unique(biome)))

#Megabiomes
biome=c("TEFO","WTFO","BOFO","TUND","STEP")
biomeSelect=c("./LDB_3/TEFO_LDB_3.csv","./LDB_3/WTFO_LDB_3.csv","./LDB_3/BOFO_LDB_3.csv","./LDB_3/TUND_LDB_3.csv","./LDB_3/STEP_LDB_3.csv")
biomeSelect=c("./LGP_3/TEFO_LGP_3.csv","./LGP_3/WTFO_LGP_3.csv","./LGP_3/BOFO_LGP_3.csv","./LGP_3/TUND_LGP_3.csv","./LGP_3/STEP_LGP_3.csv","./LGP_3/DESE_LGP_3.csv")
biomeSelect=c("./Eifel/TEFO_Eifel.csv","./Eifel/WTFO_Eifel.csv","./Eifel/BOFO_Eifel.csv","./Eifel/TUND_Eifel.csv","./Eifel/STEP_Eifel.csv","./Eifel/DESE_Eifel.csv")
biomeSelect=c("./Furamoos/TEFO_Furamoos.csv","./Furamoos/WTFO_Furamoos.csv","./Furamoos/BOFO_Furamoos.csv","./Furamoos/TUND_Furamoos.csv","./Furamoos/STEP_Furamoos.csv","./Furamoos/DESE_Furamoos.csv")
VariableClim=rep(c(1:5),times=length(unique(biome)))

for (b in 2:length(biome))
#Preparation before training
for (i in 1:5)
{{OutBiomization=read.csv2("OutModern_MegaBiomization_Modern2.csv",sep=';',dec='.')#v4 initially
SiteBiomization=read.csv2("PositionSiteCal_Output2.csv",sep=';',dec=',')
clim3<-read.csv2("ModernClimate_Class2.csv",sep=';',dec=',')
TotalSite=read.csv2("SitePlace.txt",sep=',',dec=',')
clim3$Place=TotalSite$Place
clim3=subset(clim3,clim3$Place %in% SiteBiomization$SampleName)
clim3$Biome=OutBiomization$Best
clim3=subset(clim3,clim3$Biome==biome[b])
Var=clim3[,VariableClim[i]+3] #Adapt climatic variable
SiteClimAvailable=clim3$Place[!is.na(Var)]
clim3=data.frame(clim3$Place,clim3$Longitude,clim3$Latitude,Var)
names(clim3)=c("Place","Lat","Lon","VarClim")
clim3=na.omit(clim3)

#A moduler
AssCal=read.csv2("AssCalLDB.csv",sep=";",dec=',')
AssF=read.csv2("AssFLDB-2.csv",sep=";",dec=',')
#AssF=AssF[-c(nrow(AssF)),]

rownames(AssCal)=TotalSite$Place
AssCal=AssCal[,-c(1)]
AssCal=AssCal[which(row.names(AssCal) %in% SiteClimAvailable),] 
rownames(AssF)=make.names(AssF$X2,unique=TRUE)
AssF=AssF[,-c(1)]
Age=Age_LDB$X2

AssCal=AssCal[which(row.names(AssCal) %in% SiteClimAvailable),] 
SitePolAvailable=unique(rownames(AssCal)[(AssCal!='NaN')])
AssCal=na.omit(AssCal)
clim3=clim3[which(clim3$Place %in% SitePolAvailable),] 
VarClim=as.numeric(unlist(clim3$VarClim))

#Analog=analog(AssCal,AssF,method="SQchord")
#print(dissim(Analog))
modMAT=MAT(AssCal*100,VarClim,dist.method="sq.chord",lean=FALSE,k=5)#Adapt k-analogue
predMAT<-predict(modMAT,newdata=AssF*100,bootstrap=TRUE,sse=TRUE,k=5,nboot=400)#Adapt k-analogue and nboot

if (VariableClim[i]==1)
{ReconTANN=data.frame(Age,predMAT$fit[,1],predMAT$SEP.boot[,1])}
if (VariableClim[i]==2)
{ReconPANN=data.frame(Age,predMAT$fit[,1],predMAT$SEP.boot[,1])}
if (VariableClim[i]==3)
{ReconMTCM=data.frame(Age,predMAT$fit[,1],predMAT$SEP.boot[,1])}
if (VariableClim[i]==4)
{ReconMTWM=data.frame(Age,predMAT$fit[,1],predMAT$SEP.boot[,1])}
if (VariableClim[i]==5)
{ReconGA=data.frame(Age,predMAT$fit[,1],predMAT$SEP.boot[,1])
climateRecon_WAPLS=data.frame(ReconTANN,ReconPANN,ReconMTCM,ReconMTWM,ReconGA)
write.csv2(climateRecon_WAPLS,biomeSelect[b])}}}
#names(predMAT)
#head(predMAT$v1.boot)

predMAT$SEP.boot[,1]
#plot climate reconstruction with its uncertainties
climateRecon_MAT<-data.frame(Age,predMAT$fit[,5],)
names(climateRecon_MAT)=c('Age','predfit','predSEP')
rownames(climateRecon_MAT)=NULL
write.csv(climateRecon_MAT,"Sorties_MAT_LGP.csv")
climateRecon_MAT=na.omit(climateRecon_MAT)
library(ggplot2)
ggplot(climateReconEchets_WAPLS,aes(Age,predfit))+
  ggtitle(label="Climate Reconstruction based on Pollen record",subtitle="WA-PLS/npls5-nboot1000/CPUS-15 via EMPD2/BaconR")+
  xlab("Age (Kyr)") +
  ylab("Climatic variable") +
  #geom_point(shape=8, color='black', size=1)+
  geom_line(linetype='solid')+
  geom_ribbon(aes(ymin=predfit-predSEP,ymax=predfit+predSEP, alpha = 0.2))+
  #geom_pointrange
  theme_classic()

#END