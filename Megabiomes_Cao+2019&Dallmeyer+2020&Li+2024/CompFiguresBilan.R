library(rioja)
library(analogue)
library(ggpubr)
library("cowplot")
clim3<-read.csv2("ModernClimate.csv",sep=';',dec=',')
SiteClimAvailable=clim3$X[!is.na(clim3$T_ann)]
VarClim=subset(clim3$T_ann,!is.na(clim3$T_ann))
VarClim=as.numeric(unlist(VarClim))

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
AssFFuramoos=read.csv2("AssFFuramoos.csv",sep=";",dec=',')
rownames(AssCalFuramoos)=AssCalFuramoos$X
AssCalFuramoos=AssCalFuramoos[,-c(1)]
AssCalFuramoos=AssCalFuramoos[which(row.names(AssCalFuramoos) %in% SiteClimAvailable),] 
rownames(AssFFuramoos)=AssFFuramoos$X
AssFFuramoos=AssFFuramoos[,-c(1)]

OutLDB_Crest=read.csv2("Sortie_CrestBio_LDB.csv",sep=";",dec=',')
OutLGP_Crest=read.csv2("Sortie_CrestBio_LGP.csv",sep=";",dec=',')
OutEchets_Crest=read.csv2("Sortie_CrestBio_Echets.csv",sep=";",dec=',')
OutEifel_Crest=read.csv2("Sortie_CrestBio_Eifel.csv",sep=";",dec=',')
OutFuramoos_Crest=read.csv2("Sortie_CrestBio_Furamoos.csv",sep=";",dec=',')

OutLDB_WAPLS=read.csv2("WAPLS-Biomisation-LDB.txt",sep='\t',dec=',')
OutLGP_WAPLS=read.csv2("WAPLS-Biomisation-LGP.txt",sep='\t',dec=',')
OutLGP_WAPLS=na.omit(OutLGP_WAPLS)
OutEchets_WAPLS=read.csv2("WAPLS-Biomisation-Echets.txt",sep='\t',dec=',')
OutEchets_WAPLS=na.omit(OutEchets_WAPLS)
OutEifel_WAPLS=read.csv2("WAPLS-Biomisation-Eifel.txt",sep='\t',dec=',')
OutFuramoos_WAPLS=read.csv2("WAPLS-Biomisation-Furamoos.txt",sep='\t',dec=',')

Age_LGP_Crest=OutLGP_Crest[,1]
Age_LDB_Crest=OutLDB_Crest[,1]
Age_Furamoos_Crest=OutFuramoos_Crest[,1]
Age_Echets_Crest=OutEchets_Crest[,1]
Age_Eifel_Crest=OutEifel_Crest[,1]

Age_LGP_WAPLS=OutLGP_WAPLS[,1]
Age_LDB_WAPLS=OutLDB_WAPLS[,1]
Age_Furamoos_WAPLS=OutFuramoos_WAPLS[,1]
Age_Echets_WAPLS=as.numeric(OutEchets_WAPLS[,1])
Age_Eifel_WAPLS=OutEifel_WAPLS[,1]

#Outputs 
OutMAT_TANN_Furamoos=read.csv2("Sorties_TANN_Furamoos.csv",sep=",",dec='.')
OutWAPLS_TANN_Furamoos=OutFuramoos_WAPLS[,2:3]
OutCrest_TANN_Furamoos=OutFuramoos_Crest[,2:4]
OutMAT_PANN_Furamoos=read.csv2("Sorties_PANN_Furamoos.csv",sep=",",dec='.')
OutWAPLS_PANN_Furamoos=OutFuramoos_WAPLS[,4:5]
OutCrest_PANN_Furamoos=OutFuramoos_Crest[,5:7]
OutMAT_GA_Furamoos=read.csv2("Sorties_GA_Furamoos.csv",sep=",",dec='.')
OutWAPLS_GA_Furamoos=OutFuramoos_WAPLS[,6:7]
OutCrest_GA_Furamoos=OutFuramoos_Crest[,8:10]

OutMAT_TANN_Eifel=read.csv2("Sorties_TANN_Eifel.csv",sep=",",dec='.')
OutWAPLS_TANN_Eifel=OutEifel_WAPLS[,2:3]
OutCrest_TANN_Eifel=OutEifel_Crest[,2:4]
OutMAT_PANN_Eifel=read.csv2("Sorties_PANN_Eifel.csv",sep=",",dec='.')
OutWAPLS_PANN_Eifel=OutEifel_WAPLS[,4:5]
OutCrest_PANN_Eifel=OutEifel_Crest[,5:7]
OutMAT_GA_Eifel=read.csv2("Sorties_GA_Eifel.csv",sep=",",dec='.')
OutWAPLS_GA_Eifel=OutEifel_WAPLS[,6:7]
OutCrest_GA_Eifel=OutEifel_Crest[,8:10]

OutMAT_TANN_Echets=read.csv2("Sorties_TANN_Echets.csv",sep=",",dec='.')
OutWAPLS_TANN_Echets=OutEchets_WAPLS[,2:3]
OutCrest_TANN_Echets=OutEchets_Crest[,2:4]
OutMAT_PANN_Echets=read.csv2("Sorties_PANN_Echets.csv",sep=",",dec='.')
OutWAPLS_PANN_Echets=OutEchets_WAPLS[,4:5]
OutCrest_PANN_Echets=OutEchets_Crest[,5:7]
OutMAT_GA_Echets=read.csv2("Sorties_GA_Echets.csv",sep=",",dec='.')
OutWAPLS_GA_Echets=OutEchets_WAPLS[,6:7]
OutCrest_GA_Echets=OutEchets_Crest[,8:10]

OutMAT_TANN_LGP=read.csv2("Sorties_TANN_LGP.csv",sep=",",dec='.')
OutWAPLS_TANN_LGP=OutLGP_WAPLS[,2:3]
OutCrest_TANN_LGP=OutLGP_Crest[,2:4]
OutMAT_PANN_LGP=read.csv2("Sorties_PANN_LGP.csv",sep=",",dec='.')
OutWAPLS_PANN_LGP=OutLGP_WAPLS[,4:5]
OutCrest_PANN_LGP=OutLGP_Crest[,5:7]
OutMAT_GA_LGP=read.csv2("Sorties_GA_LGP.csv",sep=",",dec='.')
OutWAPLS_GA_LGP=OutLGP_WAPLS[,6:7]
OutCrest_GA_LGP=OutLGP_Crest[,8:10]

OutMAT_TANN_LDB=read.csv2("Sorties_TANN_LDB.csv",sep=",",dec='.')
OutWAPLS_TANN_LDB=OutLDB_WAPLS[,2:3]
OutCrest_TANN_LDB=OutLDB_Crest[,2:4]
OutMAT_PANN_LDB=read.csv2("Sorties_PANN_LDB.csv",sep=",",dec='.')
OutWAPLS_PANN_LDB=OutLDB_WAPLS[,4:5]
OutCrest_PANN_LDB=OutLDB_Crest[,5:7]
OutMAT_GA_LDB=read.csv2("Sorties_GA_LDB.csv",sep=",",dec='.')
OutWAPLS_GA_LDB=OutLDB_WAPLS[,6:7]
OutCrest_GA_LDB=OutLDB_Crest[,8:10]

Age_LGP=read.csv2("ModeleAge_LGP.txt",sep=',',dec='.')
Age_LDB=read.csv2("ModeleAge_LDB.csv",sep=';',dec=',')
Age_Furamoos=read.csv2("ModeleAge_Furamoos.csv",sep=',',dec='.')
Age_Echets=read.csv2("ModeleAge_Echets.csv",sep=',',dec='.')
Age_Eifel=read.csv2("ModeleAge_Eifel.csv")

climateMAT_TANN_LDB=data.frame(Age_LDB$Age_Bacon/1E3,OutMAT_TANN_LDB[,3:4])
colnames(climateMAT_TANN_LDB)=c("Age",'predMAT','predseeMAT')
climate_TANN_LDB=na.omit(climateMAT_TANN_LDB)
climateWAPLS_TANN_LDB=data.frame(Age_LDB_WAPLS/1E3,OutWAPLS_TANN_LDB)
colnames(climateWAPLS_TANN_LDB)=c("Age",'predWAPLS','predseeWAPLS')
climateCREST_TANN_LDB=data.frame(Age_LDB_Crest/1E3,OutCrest_TANN_LDB)                                  
colnames(climateCREST_TANN_LDB)=c("Age",'predCREST','PpredseeCREST','NpredseeCREST')
climateMAT_PANN_LDB=data.frame(Age_LDB$Age_Bacon/1E3,OutMAT_PANN_LDB[,3:4])
colnames(climateMAT_PANN_LDB)=c("Age",'predMAT','predseeMAT')
climate_PANN_LDB=na.omit(climateMAT_PANN_LDB)
climateWAPLS_PANN_LDB=data.frame(Age_LDB_WAPLS/1E3,OutWAPLS_PANN_LDB)
colnames(climateWAPLS_PANN_LDB)=c("Age",'predWAPLS','predseeWAPLS')
climateCREST_PANN_LDB=data.frame(Age_LDB_Crest/1E3,OutCrest_PANN_LDB)                                  
colnames(climateCREST_PANN_LDB)=c("Age",'predCREST','PpredseeCREST','NpredseeCREST')
climateMAT_GA_LDB=data.frame(Age_LDB$Age_Bacon/1E3,OutMAT_GA_LDB[,3:4])
colnames(climateMAT_GA_LDB)=c("Age",'predMAT','predseeMAT')
climate_GA_LDB=na.omit(climateMAT_GA_LDB)
climateWAPLS_GA_LDB=data.frame(Age_LDB_WAPLS/1E3,OutWAPLS_GA_LDB)
colnames(climateWAPLS_GA_LDB)=c("Age",'predWAPLS','predseeWAPLS')
climateCREST_GA_LDB=data.frame(Age_LDB_Crest/1E3,OutCrest_GA_LDB)                                  
colnames(climateCREST_GA_LDB)=c("Age",'predCREST','PpredseeCREST','NpredseeCREST')

climateMAT_TANN_LGP=data.frame(Age_LGP$X2/1E3,OutMAT_TANN_LGP[,3:4])
colnames(climateMAT_TANN_LGP)=c("Age",'predMAT','predseeMAT')
climate_TANN_LGP=na.omit(climateMAT_TANN_LGP)
climateWAPLS_TANN_LGP=data.frame(Age_LGP_WAPLS/1E3,OutWAPLS_TANN_LGP)
colnames(climateWAPLS_TANN_LGP)=c("Age",'predWAPLS','predseeWAPLS')
climateCREST_TANN_LGP=data.frame(Age_LGP_Crest/1E3,OutCrest_TANN_LGP)                                  
colnames(climateCREST_TANN_LGP)=c("Age",'predCREST','PpredseeCREST','NpredseeCREST')
climateMAT_PANN_LGP=data.frame(Age_LGP$X2/1E3,OutMAT_PANN_LGP[,3:4])
colnames(climateMAT_PANN_LGP)=c("Age",'predMAT','predseeMAT')
climate_PANN_LGP=na.omit(climateMAT_PANN_LGP)
climateWAPLS_PANN_LGP=data.frame(Age_LGP_WAPLS/1E3,OutWAPLS_PANN_LGP)
colnames(climateWAPLS_PANN_LGP)=c("Age",'predWAPLS','predseeWAPLS')
climateCREST_PANN_LGP=data.frame(Age_LGP_Crest/1E3,OutCrest_PANN_LGP)                                  
colnames(climateCREST_PANN_LGP)=c("Age",'predCREST','PpredseeCREST','NpredseeCREST')
climateMAT_GA_LGP=data.frame(Age_LGP$X2/1E3,OutMAT_GA_LGP[,3:4])
colnames(climateMAT_GA_LGP)=c("Age",'predMAT','predseeMAT')
climate_GA_LGP=na.omit(climateMAT_GA_LGP)
climateWAPLS_GA_LGP=data.frame(Age_LGP_WAPLS/1E3,OutWAPLS_GA_LGP)
colnames(climateWAPLS_GA_LGP)=c("Age",'predWAPLS','predseeWAPLS')
climateCREST_GA_LGP=data.frame(Age_LGP_Crest/1E3,OutCrest_GA_LGP)                                  
colnames(climateCREST_GA_LGP)=c("Age",'predCREST','PpredseeCREST','NpredseeCREST')

climateMAT_TANN_Furamoos=data.frame(Age_Furamoos$X2/1E3,OutMAT_TANN_Furamoos[,3:4])
colnames(climateMAT_TANN_Furamoos)=c("Age",'predMAT','predseeMAT')
climate_TANN_Furamoos=na.omit(climateMAT_TANN_Furamoos)
climateWAPLS_TANN_Furamoos=data.frame(Age_Furamoos_WAPLS/1E3,OutWAPLS_TANN_Furamoos)
colnames(climateWAPLS_TANN_Furamoos)=c("Age",'predWAPLS','predseeWAPLS')
climateCREST_TANN_Furamoos=data.frame(Age_Furamoos_Crest/1E3,OutCrest_TANN_Furamoos)                                  
colnames(climateCREST_TANN_Furamoos)=c("Age",'predCREST','PpredseeCREST','NpredseeCREST')
climateMAT_PANN_Furamoos=data.frame(Age_Furamoos$X2/1E3,OutMAT_PANN_Furamoos[,3:4])
colnames(climateMAT_PANN_Furamoos)=c("Age",'predMAT','predseeMAT')
climate_PANN_Furamoos=na.omit(climateMAT_PANN_Furamoos)
climateWAPLS_PANN_Furamoos=data.frame(Age_Furamoos_WAPLS/1E3,OutWAPLS_PANN_Furamoos)
colnames(climateWAPLS_PANN_Furamoos)=c("Age",'predWAPLS','predseeWAPLS')
climateCREST_PANN_Furamoos=data.frame(Age_Furamoos_Crest/1E3,OutCrest_PANN_Furamoos)                                  
colnames(climateCREST_PANN_Furamoos)=c("Age",'predCREST','PpredseeCREST','NpredseeCREST')
climateMAT_GA_Furamoos=data.frame(Age_Furamoos$X2/1E3,OutMAT_GA_Furamoos[,3:4])
colnames(climateMAT_GA_Furamoos)=c("Age",'predMAT','predseeMAT')
climate_GA_Furamoos=na.omit(climateMAT_GA_Furamoos)
climateWAPLS_GA_Furamoos=data.frame(Age_Furamoos_WAPLS/1E3,OutWAPLS_GA_Furamoos)
colnames(climateWAPLS_GA_Furamoos)=c("Age",'predWAPLS','predseeWAPLS')
climateCREST_GA_Furamoos=data.frame(Age_Furamoos_Crest/1E3,OutCrest_GA_Furamoos)                                  
colnames(climateCREST_GA_Furamoos)=c("Age",'predCREST','PpredseeCREST','NpredseeCREST')

climateMAT_TANN_Eifel=data.frame(Age_Eifel$Age,OutMAT_TANN_Eifel[,3:4])
colnames(climateMAT_TANN_Eifel)=c("Age",'predMAT','predseeMAT')
climate_TANN_Eifel=na.omit(climateMAT_TANN_Eifel)
climateWAPLS_TANN_Eifel=data.frame(Age_Eifel_WAPLS,OutWAPLS_TANN_Eifel)
colnames(climateWAPLS_TANN_Eifel)=c("Age",'predWAPLS','predseeWAPLS')
climateCREST_TANN_Eifel=data.frame(Age_Eifel_Crest,OutCrest_TANN_Eifel)                                  
colnames(climateCREST_TANN_Eifel)=c("Age",'predCREST','PpredseeCREST','NpredseeCREST')
climateMAT_PANN_Eifel=data.frame(Age_Eifel$Age,OutMAT_PANN_Eifel[,3:4])
colnames(climateMAT_PANN_Eifel)=c("Age",'predMAT','predseeMAT')
climate_PANN_Eifel=na.omit(climateMAT_PANN_Eifel)
climateWAPLS_PANN_Eifel=data.frame(Age_Eifel_WAPLS,OutWAPLS_PANN_Eifel)
colnames(climateWAPLS_PANN_Eifel)=c("Age",'predWAPLS','predseeWAPLS')
climateCREST_PANN_Eifel=data.frame(Age_Eifel_Crest,OutCrest_PANN_Eifel)                                  
colnames(climateCREST_PANN_Eifel)=c("Age",'predCREST','PpredseeCREST','NpredseeCREST')
climateMAT_GA_Eifel=data.frame(Age_Eifel$Age,OutMAT_GA_Eifel[,3:4])
colnames(climateMAT_GA_Eifel)=c("Age",'predMAT','predseeMAT')
climate_GA_Eifel=na.omit(climateMAT_GA_Eifel)
climateWAPLS_GA_Eifel=data.frame(Age_Eifel_WAPLS,OutWAPLS_GA_Eifel)
colnames(climateWAPLS_GA_Eifel)=c("Age",'predWAPLS','predseeWAPLS')
climateCREST_GA_Eifel=data.frame(Age_Eifel_Crest,OutCrest_GA_Eifel)                                  
colnames(climateCREST_GA_Eifel)=c("Age",'predCREST','PpredseeCREST','NpredseeCREST')

ModeleAge_EchetsInt=read.csv2("ModeleAge_EchetsNOTCalibrated.csv",sep=';',dec='.')
climateMAT_TANN_Echets=data.frame(Age_Echets$X2/1E3,OutMAT_TANN_Echets[,3:4])
colnames(climateMAT_TANN_Echets)=c("Age",'predMAT','predseeMAT')
climate_TANN_Echets=na.omit(climateMAT_TANN_Echets)
climateWAPLS_TANN_Echets=data.frame(Age_Echets_WAPLS/1E3,OutWAPLS_TANN_Echets)
colnames(climateWAPLS_TANN_Echets)=c("Age",'predWAPLS','predseeWAPLS')
climateCREST_TANN_Echets=data.frame(Age_Echets_Crest/1E3,OutCrest_TANN_Echets)                                  
colnames(climateCREST_TANN_Echets)=c("Age",'predCREST','PpredseeCREST','NpredseeCREST')
climateMAT_PANN_Echets=data.frame(Age_Echets$X2/1E3,OutMAT_PANN_Echets[,3:4])
colnames(climateMAT_PANN_Echets)=c("Age",'predMAT','predseeMAT')
climate_PANN_Echets=na.omit(climateMAT_PANN_Echets)
climateWAPLS_PANN_Echets=data.frame(Age_Echets_WAPLS/1E3,OutWAPLS_PANN_Echets)
colnames(climateWAPLS_PANN_Echets)=c("Age",'predWAPLS','predseeWAPLS')
climateCREST_PANN_Echets=data.frame(Age_Echets_Crest/1E3,OutCrest_PANN_Echets)                                  
colnames(climateCREST_PANN_Echets)=c("Age",'predCREST','PpredseeCREST','NpredseeCREST')
climateMAT_GA_Echets=data.frame(Age_Echets$X2/1E3,OutMAT_GA_Echets[,3:4])
colnames(climateMAT_GA_Echets)=c("Age",'predMAT','predseeMAT')
climate_GA_Echets=na.omit(climateMAT_GA_Echets)
climateWAPLS_GA_Echets=data.frame(Age_Echets_WAPLS/1E3,OutWAPLS_GA_Echets)
colnames(climateWAPLS_GA_Echets)=c("Age",'predWAPLS','predseeWAPLS')
climateCREST_GA_Echets=data.frame(Age_Echets_Crest/1E3,OutCrest_GA_Echets)                                  
colnames(climateCREST_GA_Echets)=c("Age",'predCREST','PpredseeCREST','NpredseeCREST')

library(dplyr)
#function(climateMAT_TANN,)
n=full_join(climateMAT_TANN_Eifel,climateWAPLS_TANN_Eifel,by=c('Age'))
climate_TANN_Eifel=full_join(n,climateCREST_TANN_Eifel,by=c('Age'))
colnames(climate_TANN_Eifel)=c("Age","predMAT","predseeMAT","predWAPLS" ,"predseeWAPLS", "predCREST","PpredseeCREST","NpredseeCREST")

n=full_join(climateMAT_TANN_Echets,climateWAPLS_TANN_Echets,by=c('Age'))
climate_TANN_Echets=full_join(n,climateCREST_TANN_Echets,by=c('Age'))
colnames(climate_TANN_Echets)=c("Age","predMAT","predseeMAT","predWAPLS" ,"predseeWAPLS", "predCREST","PpredseeCREST","NpredseeCREST")

n=full_join(climateMAT_TANN_Furamoos,climateWAPLS_TANN_Furamoos,by=c('Age'))
climate_TANN_Furamoos=full_join(n,climateCREST_TANN_Furamoos,by=c('Age'))
colnames(climate_TANN_Furamoos)=c("Age","predMAT","predseeMAT","predWAPLS" ,"predseeWAPLS", "predCREST","PpredseeCREST","NpredseeCREST")

n=full_join(climateMAT_TANN_LGP,climateWAPLS_TANN_LGP,by=c('Age'))
climate_TANN_LGP=full_join(n,climateCREST_TANN_LGP,by=c('Age'))
colnames(climate_TANN_LGP)=c("Age","predMAT","predseeMAT","predWAPLS" ,"predseeWAPLS", "predCREST","PpredseeCREST","NpredseeCREST")

n=full_join(climateMAT_TANN_LDB,climateWAPLS_TANN_LDB,by=c('Age'))
climate_TANN_LDB=full_join(n,climateCREST_TANN_LDB,by=c('Age'))
colnames(climate_TANN_LDB)=c("Age","predMAT","predseeMAT","predWAPLS" ,"predseeWAPLS", "predCREST","PpredseeCREST","NpredseeCREST")

#%%Function to the the future-------------
vectorAge=rep(unlist(climate_TANN_Eifel$Age),each=3)
vectorPred=data.frame(matrix(NA,ncol=1,nrow=3*length(climate_TANN_Eifel$Age)))
n=0
for (i in 1:(length(climate_TANN_Eifel$Age)))
{v=i+n
a=v+1
b=v+2
#c=i+3
#d=i+4
#e=i+5
#f=i+6
#g=i+7
#h=i+8
vectorPred[v,]=climate_TANN_Eifel$predMAT[i]
print(climate_TANN_Eifel$predMAT[i])
#vectorPred[a,]=climate_TANN_Eifel$predMAT[i]+climate_TANN_Eifel$predseeMAT[i]
#vectorPred[b,]=climate_TANN_Eifel$predMAT[i]-climate_TANN_Eifel$predseeMAT[i]
vectorPred[a,]=climate_TANN_Eifel$predWAPLS[i]
#vectorPred[d,]=climate_TANN_Eifel$predWAPLS[i]+climate_TANN_Eifel$predseeWAPLS[i]
#vectorPred[e,]=climate_TANN_Eifel$predWAPLS[i]-climate_TANN_Eifel$predseeWAPLS[i]
vectorPred[b,]=climate_TANN_Eifel$predCREST[i]
n=n+2}
#vectorPred[g,]=climate_TANN_Eifel$predCREST[i]+climate_TANN_Eifel$predseeCREST[i]
#vectorPred[h,]=climate_TANN_Eifel$predCREST[i]-climate_TANN_Eifel$predseeCREST[i]
group=rep(c("predMAT","predWAPLS","predCREST"),times=length(climate_TANN_Eifel$Age))
data=data.frame(vectorAge,vectorPred,group)
data2NA=data.frame(matrix(NA,ncol=2,nrow=length(climate_TANN_Eifel$Age)))
data2=na.omit(data.frame(climate_TANN_Eifel$Age,climate_TANN_Eifel$predseeMAT))
data2NA[(1:nrow(data2)),]=data2[(1:nrow(data2)),]

data3NA=data.frame(matrix(NA,ncol=2,nrow=length(climate_TANN_Eifel$Age)))
data3=na.omit(data.frame(climate_TANN_Eifel$Age,climate_TANN_Eifel$predseeWAPLS))
data3NA[(1:nrow(data3)),]=data2[(1:nrow(data3)),]

data4NA=data.frame(matrix(NA,ncol=2,nrow=length(climate_TANN_Eifel$Age)))
data4=na.omit(data.frame(climate_TANN_Eifel$Age,climate_TANN_Eifel$predseeCREST))
data4NA[(1:nrow(data4)),]=data2[(1:nrow(data4)),]

dataFINAL=data.frame(data,data2NA,data3NA,data4NA)
#dataFINAL=na.omit(dataFINAL)

#data1=merge(data,climate_TANN_Eifel[,c(1,3,5,7)])
#colnames(dataFINAL)=c('x','y',"group","AgeMAT",'predseeMAT',"AgeWAPLS",'predseeWAPLS',"AgeCREST",'predseeCREST')
#return(data)


vectorPred=c(vectorPred)
Character=rep(c('predMAT',"predWAPLS","predCREST"),time=length(climate_TANN_Eifel$Age))

#Représentation
layout(matrix(1:4, ncol = 1))
Eifel_TANN=ggplot(climate_TANN_Eifel,aes(Age,predMAT))+
  xlab("Age (kyr)") +
  ylab("Mean Annual Temperature (°C)") +
  geom_line(data=subset(climate_TANN_Eifel,!is.na(predMAT)),aes(y=predMAT),color='red')+
  geom_ribbon(aes(ymin=predMAT-predseeMAT,ymax=predMAT+predseeMAT),na.rm=TRUE,alpha = 0.3)+
  geom_line(data=subset(climate_TANN_Eifel,!is.na(predWAPLS)),aes(y=predWAPLS),color='blue')+
  geom_ribbon(aes(ymin=predWAPLS-predseeWAPLS,ymax=predWAPLS+predseeWAPLS), na.rm=TRUE,alpha = 0.2)+
  geom_line(data=subset(climate_TANN_Eifel,!is.na(predCREST)),aes(y=predCREST),color='green')+
  geom_ribbon(aes(ymin=(NpredseeCREST),ymax=(PpredseeCREST)),na.rm=TRUE,alpha = 0.1)+
  xlim(0,40)+
  ylim(-20,20)+
  theme(panel.background = element_blank(),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

Furamoos_TANN=ggplot(climate_TANN_Furamoos,aes(Age,predMAT))+
  xlab("Age (kyr)") +
  ylab("Mean Annual Temperature (°C)") +
  geom_line(data=subset(climate_TANN_Furamoos,!is.na(predMAT)),aes(y=predMAT),color='red')+
  geom_ribbon(aes(ymin=predMAT-predseeMAT,ymax=predMAT+predseeMAT), na.rm=TRUE,alpha = 0.3)+
  geom_line(data=subset(climate_TANN_Furamoos,!is.na(predWAPLS)),aes(y=predWAPLS),color='blue')+
  geom_ribbon(aes(ymin=predWAPLS-predseeWAPLS,ymax=predWAPLS+predseeWAPLS),na.rm=TRUE,alpha = 0.2)+
  geom_line(data=subset(climate_TANN_Furamoos,!is.na(predCREST)),aes(y=predCREST),color='green')+
  geom_ribbon(aes(ymin=(NpredseeCREST),ymax=(PpredseeCREST)),na.rm=TRUE, alpha = 0.1)+
  xlim(0,40)+
  ylim(-20,20)+
  theme(panel.background = element_blank(),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

LGP_TANN=ggplot(climate_TANN_LGP,aes(Age,predMAT))+
  xlab("Age (kyr)") +
  ylab("Mean Annual Temperature (°C)") +
  geom_line(data=subset(climate_TANN_LGP,!is.na(predMAT)),aes(y=predMAT),color='red')+
  geom_ribbon(aes(ymin=predMAT-predseeMAT,ymax=predMAT+predseeMAT), na.rm=TRUE,alpha = 0.3)+
  geom_line(data=subset(climate_TANN_LGP,!is.na(predWAPLS)),aes(y=predWAPLS),color='blue')+
  geom_ribbon(aes(ymin=predWAPLS-predseeWAPLS,ymax=predWAPLS+predseeWAPLS), na.rm=TRUE,alpha = 0.2)+
  geom_line(data=subset(climate_TANN_LGP,!is.na(predCREST)),aes(y=predCREST),color='green')+
  geom_ribbon(aes(ymin=(NpredseeCREST),ymax=(PpredseeCREST)), na.rm=TRUE,alpha = 0.1)+
  xlim(0,40)+
  ylim(-20,20)+
  theme(panel.background = element_blank(),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

Echets_TANN=ggplot(climate_TANN_Echets,aes(Age,predMAT))+
  xlab("Age (kyr)") +
  ylab("Mean Annual Temperature (°C)") +
  geom_line(data=subset(climate_TANN_Echets,!is.na(predMAT)),aes(y=predMAT),color='red')+
  geom_ribbon(aes(ymin=predMAT-predseeMAT,ymax=predMAT+predseeMAT),na.rm=TRUE, alpha = 0.3)+
  geom_line(data=subset(climate_TANN_Echets,!is.na(predWAPLS)),aes(y=predWAPLS),color='blue')+
  geom_ribbon(aes(ymin=predWAPLS-predseeWAPLS,ymax=predWAPLS+predseeWAPLS), na.rm=TRUE,alpha = 0.2)+
  geom_line(data=subset(climate_TANN_Echets,!is.na(predCREST)),aes(y=predCREST),color='green')+
  geom_ribbon(aes(ymin=(NpredseeCREST),ymax=(PpredseeCREST)),na.rm=TRUE, alpha = 0.1)+
  xlim(0,40)+
  ylim(-20,20)+
  theme(panel.background = element_blank(),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

LDB_TANN=ggplot(climate_TANN_LDB,aes(Age,predMAT))+
  xlab("Age (kyr)") +
  ylab("Mean Annual Temperature (°C)") +
  geom_line(data=subset(climate_TANN_LDB,!is.na(predMAT)),aes(y=predMAT),color='red')+
  geom_ribbon(aes(ymin=predMAT-predseeMAT,ymax=predMAT+predseeMAT), na.rm=TRUE,alpha = 0.3)+
  geom_line(data=subset(climate_TANN_LDB,!is.na(predWAPLS)),aes(y=predWAPLS),color='blue')+
  geom_ribbon(aes(ymin=predWAPLS-predseeWAPLS,ymax=predWAPLS+predseeWAPLS), na.rm=TRUE,alpha = 0.2)+
  geom_line(data=subset(climate_TANN_LDB,!is.na(predCREST)),aes(y=predCREST),color='green')+
  geom_ribbon(aes(ymin=(NpredseeCREST),ymax=(PpredseeCREST)),na.rm=TRUE,alpha = 0.1)+
  xlim(0,40)+
  ylim(-20,20)+
  theme(panel.background = element_blank(),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

plot_grid(Eifel_TANN, Furamoos_TANN,LGP_TANN,Echets_TANN, LDB_TANN + rremove("x.text"), 
          ncol = 1, nrow = 5)
#END
#PANN
climate_PANN_LDB=data.frame(Age_LDB$Age_Bacon/1E3,OutMAT_PANN_LDB[,3:4],OutWAPLS_PANN_LDB[,3:4])
colnames(climate_PANN_LDB)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_PANN_LDB=na.omit(climate_PANN_LDB)
climate_PANN_LDB=data.frame(Age_LDB$Age_Bacon/1E3,OutMAT_PANN_LDB[,3:4],OutWAPLS_PANN_LDB[,3:4])
colnames(climate_PANN_LDB)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_PANN_LDB=na.omit(climate_PANN_LDB)
climate_GA_LDB=data.frame(Age_LDB$Age_Bacon/1E3,OutMAT_GA_LDB[,3:4],OutWAPLS_GA_LDB[,3:4])
colnames(climate_GA_LDB)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_LDB=na.omit(climate_GA_LDB)

climate_PANN_LGP=data.frame(Age_LGP$X2/1E3,OutMAT_PANN_LGP[,3:4],OutWAPLS_PANN_LGP[,3:4])
colnames(climate_PANN_LGP)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_PANN_LGP=na.omit(climate_PANN_LGP)
climate_PANN_LGP=data.frame(Age_LGP$X2/1E3,OutMAT_PANN_LGP[,3:4],OutWAPLS_PANN_LGP[,3:4])
colnames(climate_PANN_LGP)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_PANN_LGP=na.omit(climate_PANN_LGP)
climate_GA_LGP=data.frame(Age_LGP$X2/1E3,OutMAT_GA_LGP[,3:4],OutWAPLS_GA_LGP[,3:4])
colnames(climate_GA_LGP)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_LDB=na.omit(climate_GA_LGP)

climate_PANN_Furamoos=data.frame(Age_Furamoos$X2/1E3,OutMAT_PANN_Furamoos[,3:4],OutWAPLS_PANN_Furamoos[,3:4])
colnames(climate_PANN_Furamoos)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_PANN_Furamoos=na.omit(climate_PANN_Furamoos)
climate_PANN_Furamoos=data.frame(Age_Furamoos$X2/1E3,OutMAT_PANN_Furamoos[,3:4],OutWAPLS_PANN_Furamoos[,3:4])
colnames(climate_PANN_Furamoos)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_PANN_Furamoos=na.omit(climate_PANN_Furamoos)
climate_GA_Furamoos=data.frame(Age_Furamoos$X2/1E3,OutMAT_GA_Furamoos[,3:4],OutWAPLS_GA_Furamoos[,3:4])
colnames(climate_GA_Furamoos)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_Furamoos=na.omit(climate_GA_Furamoos)

climate_PANN_Eifel=data.frame(Age_Eifel$Age,OutMAT_PANN_Eifel[,3:4],OutWAPLS_PANN_Eifel[,3:4])
colnames(climate_PANN_Eifel)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_PANN_Eifel=na.omit(climate_PANN_Eifel)
climate_PANN_Eifel=data.frame(Age_Eifel$Age,OutMAT_PANN_Eifel[,3:4],OutWAPLS_PANN_Eifel[,3:4])
colnames(climate_PANN_Eifel)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_PANN_Eifel=na.omit(climate_PANN_Eifel)
climate_GA_Eifel=data.frame(Age_Eifel$Age,OutMAT_GA_Eifel[,3:4],OutWAPLS_GA_Eifel[,3:4])
colnames(climate_GA_Eifel)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_Eifel=na.omit(climate_GA_Eifel)

ModeleAge_EchetsInt=read.csv2("ModeleAge_EchetsNOTCalibrated.csv",sep=';',dec='.')
climate_PANN_Echets=data.frame(ModeleAge_EchetsInt$NA..1,OutMAT_PANN_Echets[,3:4],OutWAPLS_PANN_Echets[,3:4])
colnames(climate_PANN_Echets)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_PANN_Echets=na.omit(climate_PANN_Echets)
climate_PANN_Echets=data.frame(ModeleAge_EchetsInt$NA..1,OutMAT_PANN_Echets[,3:4],OutWAPLS_PANN_Echets[,3:4])
colnames(climate_PANN_Echets)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_PANN_Echets=na.omit(climate_PANN_Echets)
climate_GA_Echets=data.frame(ModeleAge_EchetsInt$NA..1,OutMAT_GA_Echets[,3:4],OutWAPLS_GA_Echets[,3:4])
colnames(climate_GA_Echets)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_Echets=na.omit(climate_GA_Echets)

n=full_join(climateMAT_PANN_Eifel,climateWAPLS_PANN_Eifel,by=c('Age'))
climate_PANN_Eifel=full_join(n,climateCREST_PANN_Eifel,by=c('Age'))
colnames(climate_PANN_Eifel)=c("Age","predMAT","predseeMAT","predWAPLS" ,"predseeWAPLS", "predCREST","PpredseeCREST","NpredseeCREST")

n=full_join(climateMAT_PANN_Echets,climateWAPLS_PANN_Echets,by=c('Age'))
climate_PANN_Echets=full_join(n,climateCREST_PANN_Echets,by=c('Age'))
colnames(climate_PANN_Echets)=c("Age","predMAT","predseeMAT","predWAPLS" ,"predseeWAPLS", "predCREST","PpredseeCREST","NpredseeCREST")

n=full_join(climateMAT_PANN_Furamoos,climateWAPLS_PANN_Furamoos,by=c('Age'))
climate_PANN_Furamoos=full_join(n,climateCREST_PANN_Furamoos,by=c('Age'))
colnames(climate_PANN_Furamoos)=c("Age","predMAT","predseeMAT","predWAPLS" ,"predseeWAPLS", "predCREST","PpredseeCREST","NpredseeCREST")

n=full_join(climateMAT_PANN_LGP,climateWAPLS_PANN_LGP,by=c('Age'))
climate_PANN_LGP=full_join(n,climateCREST_PANN_LGP,by=c('Age'))
colnames(climate_PANN_LGP)=c("Age","predMAT","predseeMAT","predWAPLS" ,"predseeWAPLS", "predCREST","PpredseeCREST","NpredseeCREST")

n=full_join(climateMAT_PANN_LDB,climateWAPLS_PANN_LDB,by=c('Age'))
climate_PANN_LDB=full_join(n,climateCREST_PANN_LDB,by=c('Age'))
colnames(climate_PANN_LDB)=c("Age","predMAT","predseeMAT","predWAPLS" ,"predseeWAPLS", "predCREST","PpredseeCREST","NpredseeCREST")

#Représentation
layout(matrix(1:4, ncol = 1))
Eifel_PANN=ggplot(climate_PANN_Eifel,aes(Age,predMAT))+
  xlab("Age (kyr)") +
  ylab("Mean Annual Precipitation (mm)") +
  geom_line(data=subset(climate_PANN_Eifel,!is.na(predMAT)),aes(y=predMAT),color='red')+
  geom_ribbon(aes(ymin=predMAT-predseeMAT,ymax=predMAT+predseeMAT),na.rm=TRUE,alpha = 0.3)+
  geom_line(data=subset(climate_PANN_Eifel,!is.na(predWAPLS)),aes(y=predWAPLS),color='blue')+
  geom_ribbon(aes(ymin=predWAPLS-predseeWAPLS,ymax=predWAPLS+predseeWAPLS), na.rm=TRUE,alpha = 0.2)+
  geom_line(data=subset(climate_PANN_Eifel,!is.na(predCREST)),aes(y=predCREST),color='green')+
  geom_ribbon(aes(ymin=NpredseeCREST,ymax=PpredseeCREST), na.rm=TRUE,alpha = 0.1)+
  xlim(0,40)+
  ylim(0,2500)+
  theme(panel.background = element_blank(),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

Furamoos_PANN=ggplot(climate_PANN_Furamoos,aes(Age,predMAT))+
  xlab("Age (kyr)") +
  ylab("Mean Annual Precipitation (mm)") +
  geom_line(data=subset(climate_PANN_Furamoos,!is.na(predMAT)),aes(y=predMAT),color='red')+
  geom_ribbon(aes(ymin=predMAT-predseeMAT,ymax=predMAT+predseeMAT), na.rm=TRUE,alpha = 0.3)+
  geom_line(data=subset(climate_PANN_Furamoos,!is.na(predWAPLS)),aes(y=predWAPLS),color='blue')+
  geom_ribbon(aes(ymin=predWAPLS-predseeWAPLS,ymax=predWAPLS+predseeWAPLS),na.rm=TRUE,alpha = 0.2)+
  geom_line(data=subset(climate_PANN_Furamoos,!is.na(predCREST)),aes(y=predCREST),color='green')+
  geom_ribbon(aes(ymin=NpredseeCREST,ymax=PpredseeCREST),na.rm=TRUE, alpha = 0.1)+
  xlim(0,40)+
  ylim(0,2500)+
  theme(panel.background = element_blank(),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

LGP_PANN=ggplot(climate_PANN_LGP,aes(Age,predMAT))+
  xlab("Age (kyr)") +
  ylab("Mean Annual Precipitation (mm)") +
  geom_line(data=subset(climate_PANN_LGP,!is.na(predMAT)),aes(y=predMAT),color='red')+
  geom_ribbon(aes(ymin=predMAT-predseeMAT,ymax=predMAT+predseeMAT), na.rm=TRUE,alpha = 0.3)+
  geom_line(data=subset(climate_PANN_LGP,!is.na(predWAPLS)),aes(y=predWAPLS),color='blue')+
  geom_ribbon(aes(ymin=predWAPLS-predseeWAPLS,ymax=predWAPLS+predseeWAPLS), na.rm=TRUE,alpha = 0.2)+
  geom_line(data=subset(climate_PANN_LGP,!is.na(predCREST)),aes(y=predCREST),color='green')+
  geom_ribbon(aes(ymin=NpredseeCREST,ymax=PpredseeCREST), na.rm=TRUE,alpha = 0.1)+
  xlim(0,40)+
  ylim(0,2500)+
  theme(panel.background = element_blank(),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

Echets_PANN=ggplot(climate_PANN_Echets,aes(Age,predMAT))+
  xlab("Age (kyr)") +
  ylab("Mean Annual Precipitation (mm)") +
  geom_line(data=subset(climate_PANN_Echets,!is.na(predMAT)),aes(y=predMAT),color='red')+
  geom_ribbon(aes(ymin=predMAT-predseeMAT,ymax=predMAT+predseeMAT),na.rm=TRUE, alpha = 0.3)+
  geom_line(data=subset(climate_PANN_Echets,!is.na(predWAPLS)),aes(y=predWAPLS),color='blue')+
  geom_ribbon(aes(ymin=predWAPLS-predseeWAPLS,ymax=predWAPLS+predseeWAPLS), na.rm=TRUE,alpha = 0.2)+
  geom_line(data=subset(climate_PANN_Echets,!is.na(predCREST)),aes(y=predCREST),color='green')+
  geom_ribbon(aes(ymin=NpredseeCREST,ymax=PpredseeCREST),na.rm=TRUE, alpha = 0.1)+
  xlim(0,40)+
  ylim(0,2500)+
  theme(panel.background = element_blank(),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

LDB_PANN=ggplot(climate_PANN_LDB,aes(Age,predMAT))+
  xlab("Age (kyr)") +
  ylab("Mean Annual Precipitation (mm)") +
  geom_line(data=subset(climate_PANN_LDB,!is.na(predMAT)),aes(y=predMAT),color='red')+
  geom_ribbon(aes(ymin=predMAT-predseeMAT,ymax=predMAT+predseeMAT), na.rm=TRUE,alpha = 0.3)+
  geom_line(data=subset(climate_PANN_LDB,!is.na(predWAPLS)),aes(y=predWAPLS),color='blue')+
  geom_ribbon(aes(ymin=predWAPLS-predseeWAPLS,ymax=predWAPLS+predseeWAPLS), na.rm=TRUE,alpha = 0.2)+
  geom_line(data=subset(climate_PANN_LDB,!is.na(predCREST)),aes(y=predCREST),color='green')+
  geom_ribbon(aes(ymin=NpredseeCREST,ymax=PpredseeCREST),na.rm=TRUE,alpha = 0.1)+
  xlim(0,40)+
  ylim(0,2500)+
  theme(panel.background = element_blank(),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

plot_grid(Eifel_PANN, Furamoos_PANN,LGP_PANN,Echets_PANN, LDB_PANN + rremove("x.text"), 
          ncol = 1, nrow = 5)
#END

#GA
climate_GA_LDB=data.frame(Age_LDB$Age_Bacon/1E3,OutMAT_GA_LDB[,3:4],OutWAPLS_GA_LDB[,3:4])
colnames(climate_GA_LDB)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_LDB=na.omit(climate_GA_LDB)
climate_GA_LDB=data.frame(Age_LDB$Age_Bacon/1E3,OutMAT_GA_LDB[,3:4],OutWAPLS_GA_LDB[,3:4])
colnames(climate_GA_LDB)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_LDB=na.omit(climate_GA_LDB)
climate_GA_LDB=data.frame(Age_LDB$Age_Bacon/1E3,OutMAT_GA_LDB[,3:4],OutWAPLS_GA_LDB[,3:4])
colnames(climate_GA_LDB)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_LDB=na.omit(climate_GA_LDB)

climate_GA_LGP=data.frame(Age_LGP$X2/1E3,OutMAT_GA_LGP[,3:4],OutWAPLS_GA_LGP[,3:4])
colnames(climate_GA_LGP)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_LGP=na.omit(climate_GA_LGP)
climate_GA_LGP=data.frame(Age_LGP$X2/1E3,OutMAT_GA_LGP[,3:4],OutWAPLS_GA_LGP[,3:4])
colnames(climate_GA_LGP)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_LGP=na.omit(climate_GA_LGP)
climate_GA_LGP=data.frame(Age_LGP$X2/1E3,OutMAT_GA_LGP[,3:4],OutWAPLS_GA_LGP[,3:4])
colnames(climate_GA_LGP)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_LDB=na.omit(climate_GA_LGP)

climate_GA_Furamoos=data.frame(Age_Furamoos$X2/1E3,OutMAT_GA_Furamoos[,3:4],OutWAPLS_GA_Furamoos[,3:4])
colnames(climate_GA_Furamoos)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_Furamoos=na.omit(climate_GA_Furamoos)
climate_GA_Furamoos=data.frame(Age_Furamoos$X2/1E3,OutMAT_GA_Furamoos[,3:4],OutWAPLS_GA_Furamoos[,3:4])
colnames(climate_GA_Furamoos)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_Furamoos=na.omit(climate_GA_Furamoos)
climate_GA_Furamoos=data.frame(Age_Furamoos$X2/1E3,OutMAT_GA_Furamoos[,3:4],OutWAPLS_GA_Furamoos[,3:4])
colnames(climate_GA_Furamoos)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_Furamoos=na.omit(climate_GA_Furamoos)

climate_GA_Eifel=data.frame(Age_Eifel$Age,OutMAT_GA_Eifel[,3:4],OutWAPLS_GA_Eifel[,3:4])
colnames(climate_GA_Eifel)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_Eifel=na.omit(climate_GA_Eifel)
climate_GA_Eifel=data.frame(Age_Eifel$Age,OutMAT_GA_Eifel[,3:4],OutWAPLS_GA_Eifel[,3:4])
colnames(climate_GA_Eifel)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_Eifel=na.omit(climate_GA_Eifel)
climate_GA_Eifel=data.frame(Age_Eifel$Age,OutMAT_GA_Eifel[,3:4],OutWAPLS_GA_Eifel[,3:4])
colnames(climate_GA_Eifel)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_Eifel=na.omit(climate_GA_Eifel)

ModeleAge_EchetsInt=read.csv2("ModeleAge_EchetsNOTCalibrated.csv",sep=';',dec='.')
climate_GA_Echets=data.frame(ModeleAge_EchetsInt$NA..1,OutMAT_GA_Echets[,3:4],OutWAPLS_GA_Echets[,3:4])
colnames(climate_GA_Echets)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_Echets=na.omit(climate_GA_Echets)
climate_GA_Echets=data.frame(ModeleAge_EchetsInt$NA..1,OutMAT_GA_Echets[,3:4],OutWAPLS_GA_Echets[,3:4])
colnames(climate_GA_Echets)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_Echets=na.omit(climate_GA_Echets)
climate_GA_Echets=data.frame(ModeleAge_EchetsInt$NA..1,OutMAT_GA_Echets[,3:4],OutWAPLS_GA_Echets[,3:4])
colnames(climate_GA_Echets)=c("Age",'predMAT','predseeMAT','predWAPLS','predseeWAPLS')
climate_GA_Echets=na.omit(climate_GA_Echets)

n=full_join(climateMAT_GA_Eifel,climateWAPLS_GA_Eifel,by=c('Age'))
climate_GA_Eifel=full_join(n,climateCREST_GA_Eifel,by=c('Age'))
colnames(climate_GA_Eifel)=c("Age","predMAT","predseeMAT","predWAPLS" ,"predseeWAPLS", "predCREST","PpredseeCREST","NpredseeCREST")

n=full_join(climateMAT_GA_Echets,climateWAPLS_GA_Echets,by=c('Age'))
climate_GA_Echets=full_join(n,climateCREST_GA_Echets,by=c('Age'))
colnames(climate_GA_Echets)=c("Age","predMAT","predseeMAT","predWAPLS" ,"predseeWAPLS", "predCREST","PpredseeCREST","NpredseeCREST")

n=full_join(climateMAT_GA_Furamoos,climateWAPLS_GA_Furamoos,by=c('Age'))
climate_GA_Furamoos=full_join(n,climateCREST_GA_Furamoos,by=c('Age'))
colnames(climate_GA_Furamoos)=c("Age","predMAT","predseeMAT","predWAPLS" ,"predseeWAPLS", "predCREST","PpredseeCREST","NpredseeCREST")

n=full_join(climateMAT_GA_LGP,climateWAPLS_GA_LGP,by=c('Age'))
climate_GA_LGP=full_join(n,climateCREST_GA_LGP,by=c('Age'))
colnames(climate_GA_LGP)=c("Age","predMAT","predseeMAT","predWAPLS" ,"predseeWAPLS", "predCREST","PpredseeCREST","NpredseeCREST")

n=full_join(climateMAT_GA_LDB,climateWAPLS_GA_LDB,by=c('Age'))
climate_GA_LDB=full_join(n,climateCREST_GA_LDB,by=c('Age'))
colnames(climate_GA_LDB)=c("Age","predMAT","predseeMAT","predWAPLS" ,"predseeWAPLS", "predCREST","PpredseeCREST","NpredseeCREST")

#Représentation
layout(matrix(1:4, ncol = 1))
Eifel_GA=ggplot(climate_GA_Eifel,aes(Age,predMAT))+
  xlab("Age (kyr)") +
  ylab("Aridity Index") +
  geom_line(data=subset(climate_GA_Eifel,!is.na(predMAT)),aes(y=predMAT),color='red')+
  geom_ribbon(aes(ymin=predMAT-predseeMAT,ymax=predMAT+predseeMAT),na.rm=TRUE,alpha = 0.3)+
  geom_line(data=subset(climate_GA_Eifel,!is.na(predWAPLS)),aes(y=predWAPLS),color='blue')+
  geom_ribbon(aes(ymin=predWAPLS-predseeWAPLS,ymax=predWAPLS+predseeWAPLS), na.rm=TRUE,alpha = 0.2)+
  geom_line(data=subset(climate_GA_Eifel,!is.na(predCREST)),aes(y=predCREST),color='green')+
  geom_ribbon(aes(ymin=NpredseeCREST,ymax=PpredseeCREST), na.rm=TRUE,alpha = 0.1)+
  xlim(0,40)+
  ylim(0,30000)+
  theme(panel.background = element_blank(),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

Furamoos_GA=ggplot(climate_GA_Furamoos,aes(Age,predMAT))+
  xlab("Age (kyr)") +
  ylab("Aridity Index") +
  geom_line(data=subset(climate_GA_Furamoos,!is.na(predMAT)),aes(y=predMAT),color='red')+
  geom_ribbon(aes(ymin=predMAT-predseeMAT,ymax=predMAT+predseeMAT), na.rm=TRUE,alpha = 0.3)+
  geom_line(data=subset(climate_GA_Furamoos,!is.na(predWAPLS)),aes(y=predWAPLS),color='blue')+
  geom_ribbon(aes(ymin=predWAPLS-predseeWAPLS,ymax=predWAPLS+predseeWAPLS),na.rm=TRUE,alpha = 0.2)+
  geom_line(data=subset(climate_GA_Furamoos,!is.na(predCREST)),aes(y=predCREST),color='green')+
  geom_ribbon(aes(ymin=NpredseeCREST,ymax=PpredseeCREST),na.rm=TRUE, alpha = 0.1)+
  xlim(0,40)+
  ylim(0,30000)+
  theme(panel.background = element_blank(),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

LGP_GA=ggplot(climate_GA_LGP,aes(Age,predMAT))+
  xlab("Age (kyr)") +
  ylab("Aridity Index") +
  geom_line(data=subset(climate_GA_LGP,!is.na(predMAT)),aes(y=predMAT),color='red')+
  geom_ribbon(aes(ymin=predMAT-predseeMAT,ymax=predMAT+predseeMAT), na.rm=TRUE,alpha = 0.3)+
  geom_line(data=subset(climate_GA_LGP,!is.na(predWAPLS)),aes(y=predWAPLS),color='blue')+
  geom_ribbon(aes(ymin=predWAPLS-predseeWAPLS,ymax=predWAPLS+predseeWAPLS), na.rm=TRUE,alpha = 0.2)+
  geom_line(data=subset(climate_GA_LGP,!is.na(predCREST)),aes(y=predCREST),color='green')+
  geom_ribbon(aes(ymin=NpredseeCREST,ymax=PpredseeCREST), na.rm=TRUE,alpha = 0.1)+
  xlim(0,40)+
  ylim(0,30000)+
  theme(panel.background = element_blank(),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

Echets_GA=ggplot(climate_GA_Echets,aes(Age,predMAT))+
  xlab("Age (kyr)") +
  ylab("Aridity Index") +
  geom_line(data=subset(climate_GA_Echets,!is.na(predMAT)),aes(y=predMAT),color='red')+
  geom_ribbon(aes(ymin=predMAT-predseeMAT,ymax=predMAT+predseeMAT),na.rm=TRUE, alpha = 0.3)+
  geom_line(data=subset(climate_GA_Echets,!is.na(predWAPLS)),aes(y=predWAPLS),color='blue')+
  geom_ribbon(aes(ymin=predWAPLS-predseeWAPLS,ymax=predWAPLS+predseeWAPLS), na.rm=TRUE,alpha = 0.2)+
  geom_line(data=subset(climate_GA_Echets,!is.na(predCREST)),aes(y=predCREST),color='green')+
  geom_ribbon(aes(ymin=NpredseeCREST,ymax=PpredseeCREST),na.rm=TRUE, alpha = 0.1)+
  xlim(0,40)+
  ylim(0,30000)+
  theme(panel.background = element_blank(),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

LDB_GA=ggplot(climate_GA_LDB,aes(Age,predMAT))+
  xlab("Age (kyr)") +
  ylab("Aridity Index") +
  geom_line(data=subset(climate_GA_LDB,!is.na(predMAT)),aes(y=predMAT),color='red')+
  geom_ribbon(aes(ymin=predMAT-predseeMAT,ymax=predMAT+predseeMAT), na.rm=TRUE,alpha = 0.3)+
  geom_line(data=subset(climate_GA_LDB,!is.na(predWAPLS)),aes(y=predWAPLS),color='blue')+
  geom_ribbon(aes(ymin=predWAPLS-predseeWAPLS,ymax=predWAPLS+predseeWAPLS), na.rm=TRUE,alpha = 0.2)+
  geom_line(data=subset(climate_GA_LDB,!is.na(predCREST)),aes(y=predCREST),color='green')+
  geom_ribbon(aes(ymin=NpredseeCREST,ymax=PpredseeCREST),na.rm=TRUE,alpha = 0.1)+
  xlim(0,40)+
  ylim(0,30000)+
  theme(panel.background = element_blank(),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

plot_grid(Eifel_GA, Furamoos_GA,LGP_GA,Echets_GA, LDB_GA + rremove("x.text"), 
          ncol = 1, nrow = 5)


#END

