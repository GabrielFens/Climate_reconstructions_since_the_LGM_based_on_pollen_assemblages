#Biomisation-WAPLS/Traitement
library(rioja)
library(analogue)
library(dplyr)
library(datawizard)
library(fxTWAPLS)
library(remotes)
library(rlist)

#Megabiomes
biome=c("TEFO","WTFO","BOFO","TUND","STEP")
biomeSelect=c("./LDB_3/TEFO_LDB_3.csv","./LDB_3/WTFO_LDB_3.csv","./LDB_3/BOFO_LDB_3.csv","./LDB_3/TUND_LDB_3.csv","./LDB_3/STEP_LDB_3.csv")
biomeSelect=c("./LGP_3/TEFO_LGP_3.csv","./LGP_3/WTFO_LGP_3.csv","./LGP_3/BOFO_LGP_3.csv","./LGP_3/TUND_LGP_3.csv","./LGP_3/STEP_LGP_3.csv")
biomeSelect=c("./Eifel/TEFO_Eifel.csv","./Eifel/WTFO_Eifel.csv","./Eifel/BOFO_Eifel.csv","./Eifel/TUND_Eifel.csv","./Eifel/STEP_Eifel.csv")
biomeSelect=c("./Furamoos/TEFO_Furamoos.csv","./Furamoos/WTFO_Furamoos.csv","./Furamoos/BOFO_Furamoos.csv","./Furamoos/TUND_Furamoos.csv","./Furamoos/STEP_Furamoos.csv")
VariableClim=rep(c(1:5),times=length(unique(biome)))

for (b in 1:length(biome))
{for (i in 1:5)
{OutBiomization=read.csv2("OutModern_MegaBiomization_Modern2.csv",sep=';',dec='.')#v4 initially
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
  AssCal=read.csv2("AssCalLGP2.csv",sep=";",dec=',')
  AssF=read.csv2("AssFLGP_2.csv",sep=";",dec=',')[0:27,]
  #AssF=AssF[-c(nrow(AssF)),]
  
  rownames(AssCal)=TotalSite$Place
  AssCal=AssCal[,-c(1)]
  AssCal=AssCal[which(row.names(AssCal) %in% SiteClimAvailable),] 
  rownames(AssF)=make.names(AssF$X2,unique=TRUE)
  AssF=AssF[,-c(1)]
  Age_LGP=read.csv2("ModeleAge_LGP.txt",sep=',',dec='.')
  Age_LGP=read.csv2("ModeleAge_LGP2.txt",sep=',',dec='.')
  Age_LDB=read.csv2("ModeleAge_LDB.txt",sep=',',dec='.')
  Age_Furamoos=read.csv2("ModeleAge_Furamoos.csv",sep=',',dec='.')
  Age_Echets=read.csv2("ModeleAge_Echets.txt",sep=',',dec='.')
  Age_Eifel=read.csv2("ModeleAge_Eifel.csv")
  
  AssCal=AssCal[which(row.names(AssCal) %in% SiteClimAvailable),] 
  SitePolAvailable=unique(rownames(AssCal)[(AssCal!='NaN')])
  AssCal=na.omit(AssCal)
  clim3=clim3[which(clim3$Place %in% SitePolAvailable),] 
  VarClim=as.numeric(unlist(clim3$VarClim))
  
  TaxaSelect=colnames(AssCal)[colSums(AssCal)==0]
  AssCal=data_select(AssCal,-TaxaSelect)
  #AssCal=select(AssCal,-c("Varia"))
  AssF=data_select(AssF,-TaxaSelect)
  taxa=AssCal
  modern_pollen=VarClim
  
  #A moduler
  Age=Age_LGP$X2[0:27]
  #Age=Age[1:(length(Age)-1)]
  
  fit_Tmin=fxTWAPLS::WAPLS.w(taxa, modern_pollen, nPLS = 5)
  #model=lm(fit_Tmin$fit[,2]~modern_pollen)
  #summary(model)
  #sqrt(mean(model$residuals^2))
  #fit_t_Tmin <- fxTWAPLS::TWAPLS.w(taxa, modern_pollen, nPLS = 5)
  #fit_f_Tmin <- fxTWAPLS::WAPLS.w(taxa, 
  #modern_pollen, 
  #nPLS = 5, 
  #usefx = TRUE, 
  #fx = fx_Tmin)
  #fit_tf_Tmin <- fxTWAPLS::TWAPLS.w(taxa, 
  #modern_pollen, 
  #nPLS = 5, 
  #usefx = TRUE, 
  #fx = fx_Tmin)
  #modWAPLS=fxTWAPLS::WAPLS.w(AssCal,VarClim,nPLS=5)
  #Leave-Out cross validation 
  #CPUS=16
  #modWAPLS=fxTWAPLS::cv.w(AssCal,VarClim,nPLS = 5,
  #fxTWAPLS::WAPLS.w,
  #fxTWAPLS::WAPLS.predict.w,
  #usefx=FALSE,
  #fx_method='bin',
  #cpus=CPUS)
  #Leave-Out validation - R2, RMSEP and maximum bias taking into account the geographycal and climatic data
  #point=data.frame(Samples$Longitude,Samples$Latitude)
  #names(point)=c('Long','Lat')
  #`%>%` <- magrittr::`%>%`
  #dist=fxTWAPLS::get_distance(point, cpus = CPUS)
  #rlist::list.save(dist, 'distrdata')
  #write.csv(dist, "distance.csv")
  # Get the geographically and climatically close sites
  #distA=read.csv("distance.csv", row.names = 1)
  #write(dist,file="DistanceAnalogue.csv")
  #dist=read.csv("DistanceAnalogue.csv", row.names = 1)
  #pseudo_Tmin=fxTWAPLS::get_pseudo(matrix(dist[1,]),modern_pollen,cpus=CPUS)
  #rlist::list.save(pseudo_Tmin, 'pseudo_Tmin.rdata')
  #cv_pr_Tmin=fxTWAPLS::cv.pr.w(taxa,
  #modern_pollen,
  #nPLS = 5,
  #fxTWAPLS::WAPLS.w,
  # fxTWAPLS::WAPLS.predict.w,
  # pseudo_Tmin,
  # cpus = CPUS)
  
  #Prediction and see
  pred=fxTWAPLS::WAPLS.predict.w(fit_Tmin, AssF)
  
  #Test the cross validation result
  cv_pr_Tmin=fxTWAPLS::cv.w(taxa, modern_pollen, nPLS = 5,fxTWAPLS::WAPLS.w,fxTWAPLS::WAPLS.predict.w)
  rand_pr_tf_Tmin2=fxTWAPLS::rand.t.test.w(cv_pr_Tmin,n.perm = 999)
  n=rand_pr_tf_Tmin2[,1]
  nsig=which(n==max(n))
  sse_Tmin_WAPLS=fxTWAPLS::sse.sample(modern_taxa = taxa,
                                      modern_climate = modern_pollen,
                                      fossil_taxa = AssF,
                                      trainfun = fxTWAPLS::WAPLS.w,
                                      predictfun = fxTWAPLS::WAPLS.predict.w,
                                      nboot = 1000,
                                      nPLS = 5,
                                      nsig = nsig,
                                      usefx = FALSE,
                                      fx = NA)
  if (VariableClim[b]==1)
  {ReconTANN=data.frame(Age,pred$fit[,1],sse_Tmin_WAPLS)}
  if (VariableClim[b]==2)
  {ReconPANN=data.frame(Age,pred$fit[,nsig],sse_Tmin_WAPLS)}
  if (VariableClim[b]==3)
  {ReconMTCM=data.frame(Age,pred$fit[,nsig],sse_Tmin_WAPLS)}
  if (VariableClim[b]==4)
  {ReconMTWM=data.frame(Age,pred$fit[,nsig],sse_Tmin_WAPLS)}
  if (VariableClim[b]==5)
  {ReconGA=data.frame(Age,pred$fit[,nsig],sse_Tmin_WAPLS)
  climateRecon_WAPLS=data.frame(ReconTANN,ReconPANN,ReconMTCM,ReconMTWM,ReconGA)
  #write.csv2(climateRecon_WAPLS,"WAPLS_LDB_CalGlobale.csv")
  write.csv2(climateRecon_WAPLS,biomeSelect[m])
  m=m+1}}
  #names(climateRecon_WAPLS)=c('Age','predfit','predSEP')
  #rownames(climateRecon_WAPLS)=NULL
}
