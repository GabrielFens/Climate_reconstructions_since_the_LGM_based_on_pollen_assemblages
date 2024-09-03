library(crestr)
library(rio)

#Formatage EMPD2 après biomisation
EMPD=read.csv2("EMPD_Count.csv")
Taxon=unique(EMPD[,"acc_varname"])
Site=unique(EMPD[,"SampleName"])
AssCalEMPD<-data.frame(matrix(NA,ncol=length(Taxon),nrow=length(Site)))
colnames(AssCalEMPD)=Taxon
rownames(AssCalEMPD)=Site
for(i in 1:(length(Site)))
{for (j in 1:length(Taxon))
{if (identical(sum(EMPD[,3][(EMPD[,1]==Site[i])&(EMPD[,2]==Taxon[j])]), character(0))==FALSE)
{AssCalEMPD[i,j]=sum(EMPD[,3][(EMPD[,1]==Site[i])&(EMPD[,2]==Taxon[j])])}
  else
  {AssCalEMPD[i,j]=0}
}}

#Exportation
rownames(AssCalEMPD)=Site
colnames(AssCalEMPD)=Site
M=matrix(AssCalEMPD)
write.table(AssCalEMPD,"AssCalEMPD.csv",sep=';')
write.csv(rownames(AssCalEMPD),"AssCalEMPD-row.csv")

#Normalization of calibration datasets
for(i in 1:(length(EMPD[,3])))
{EMPD[i,3]=(100*EMPD[i,3])/(sum(EMPD3[,3][(EMPD3[,1]==EMPD3[i,1])]))}
write.csv2(EMPD,"EMPD_Normalized.csv")




#Formatage environ - Modern EMPD2
Samples=read.csv2("LonLatSiteCalEMPD2.txt",sep='\t')
ModernClimate=read.csv2("ModernClimate.csv",sep=';',dec=',')
OutBiomization=read.csv2("OutModern_MegaBiomization_Modern2.csv",sep=';',dec='.')#v4 initially
SiteBiomization=read.csv2("PositionSiteCal_Output2.csv",sep=';',dec='.')


Biome=data.frame(matrix(NA,ncol=1,nrow=nrow(Samples)))
ModernData_Climate=data.frame(Samples$SiteName,ModernClimate,Biome$matrix.NA..ncol...1..nrow...nrow.Samples..)
ModernData_Biome=data.frame(OutBiomization,SiteBiomization)
for (y in 1:nrow(ModernData_Climate))
{if (ModernData_Climate$Samples.SiteName[y] %in% ModernData_Biome$SampleName)
{ModernData_Climate[y,"Biome.matrix.NA..ncol...1..nrow...nrow.Samples.."]=ModernData_Biome$Best[ModernData_Biome$SampleName==ModernData_Climate$Samples.SiteName[y]]}}
write.csv2(ModernData_Climate,"EMPD2_envi.csv")

#Firstly
#biome=c("CLDE","TAIG","CLMX","COCO","TEDE","COMX","WAMX","XERO","TUND","COST","WAST","CODE")

biome=c("TEFO","WTFO","BOFO","TUND","STEP","DESE")
biomeSelect=c("./LDB/CLDE_LDB.csv","./LDB/TAIG_LDB.csv","./LDB/PION_LDB.csv","./LDB/CLMX_LDB.csv","./LDB/COCO_LDB.csv","./LDB/TEDE_LDB.csv","./LDB/COMX_LDB.csv","./LDB/WAMX_LDB.csv","./LDB/XERO_LDB.csv","./LDB/TUND_LDB.csv","./LDB/COST_LDB.csv","./LDB/WAST_LDB.csv","./LDB/CODE_LDB.csv","./LDB/HODE_LDB.csv")
biomeSelect=c("./LGP/CLDE_LGP.csv","./LGP/TAIG_LGP.csv","./LGP/PION_LGP.csv","./LGP/CLMX_LGP.csv","./LGP/COCO_LGP.csv","./LGP/TEDE_LGP.csv","./LGP/COMX_LGP.csv","./LGP/WAMX_LGP.csv","./LGP/XERO_LGP.csv","./LGP/TUND_LGP.csv","./LGP/COST_LGP.csv","./LGP/WAST_LGP.csv","./LGP/CODE_LGP.csv","./LGP/HODE_LGP.csv")
biomeSelect=c("./Echets/CLDE_Echets.csv","./Echets/TAIG_Echets.csv","./Echets/PION_Echets.csv","./Echets/CLMX_Echets.csv","./Echets/COCO_Echets.csv","./Echets/TEDE_Echets.csv","./Echets/COMX_Echets.csv","./Echets/WAMX_Echets.csv","./Echets/XERO_Echets.csv","./Echets/TUND_Echets.csv","./Echets/COST_Echets.csv","./Echets/WAST_Echets.csv","./Echets/CODE_Echets.csv","./Echets/HODE_Echets.csv")
biomeSelect=c("./Eifel/CLDE_Eifel.csv","./Eifel/TAIG_Eifel.csv","./Eifel/PION_Eifel.csv","./Eifel/CLMX_Eifel.csv","./Eifel/COCO_Eifel.csv","./Eifel/TEDE_Eifel.csv","./Eifel/COMX_Eifel.csv","./Eifel/WAMX_Eifel.csv","./Eifel/XERO_Eifel.csv","./Eifel/TUND_Eifel.csv","./Eifel/COST_Eifel.csv","./Eifel/WAST_Eifel.csv","./Eifel/CODE_Eifel.csv","./Eifel/HODE_Eifel.csv")
biomeSelect=c("./Furamoos/CLDE_Furamoos.csv","./Furamoos/TAIG_Furamoos.csv","./Furamoos/PION_Furamoos.csv","./Furamoos/CLMX_Furamoos.csv","./Furamoos/COCO_Furamoos.csv","./Furamoos/TEDE_Furamoos.csv","./Furamoos/COMX_Furamoos.csv","./Furamoos/WAMX_Furamoos.csv","./Furamoos/XERO_Furamoos.csv","./Furamoos/TUND_Furamoos.csv","./Furamoos/COST_Furamoos.csv","./Furamoos/WAST_Furamoos.csv","./Furamoos/CODE_Furamoos.csv","./Furamoos/HODE_Furamoos.csv")
biome=c("CLDE","TAIG","PION","CLMX","COCO","TEDE","COMX","WAMX","XERO","TUND","COST","WAST","CODE","HODE")

biome=c("TEFO","WTFO","BOFO","TUND","STEP","DESE")
biomeSelect=c("./LDB_3/TEFO_LDB_3.csv","./LDB_3/WTFO_LDB_3.csv","./LDB_3/BOFO_LDB_3.csv","./LDB_3/TUND_LDB_3.csv","./LDB_3/STEP_LDB_3.csv","./LDB_3/DESE_LDB_3.csv")
biomeSelect=c("./LGP_3/TEFO_LGP_3.csv","./LGP_3/WTFO_LGP_3.csv","./LGP_3/BOFO_LGP_3.csv","./LGP_3/TUND_LGP_3.csv","./LGP_3/STEP_LGP_3.csv","./LGP_3/DESE_LGP_3.csv")
biomeSelect=c("./Eifel/TEFO_Eifel.csv","./Eifel/WTFO_Eifel.csv","./Eifel/BOFO_Eifel.csv","./Eifel/TUND_Eifel.csv","./Eifel/STEP_Eifel.csv","./Eifel/DESE_Eifel.csv")
biomeSelect=c("./Furamoos/TEFO_Furamoos.csv","./Furamoos/WTFO_Furamoos.csv","./Furamoos/BOFO_Furamoos.csv","./Furamoos/TUND_Furamoos.csv","./Furamoos/STEP_Furamoos.csv","./Furamoos/DESE_Furamoos.csv")

for (b in 1:length(biomeSelect))
{
metadata=rio::import('EMPD2-Set_environ.xlsx', skip=1)
df=rio::import('EMPD2-Set.xlsx')
crest_df<-read.csv2("AssFLDB.csv",sep=';',dec=',')
#crest_df=crest_df[,1:length(crest_df)]
#names(crest_df[,"Saxifraga"])="Saxifragaceae"
metadata[metadata=='NA']=NA
metadata=na.omit(metadata)
metadata[,7]=as.numeric(metadata[,7])
metadata[,8]=as.numeric(metadata[,8])
metadata[,9]=as.numeric(metadata[,9])
metadata[,10]=as.numeric(metadata[,10])
metadata[,11]=as.numeric(metadata[,11])
#metadata[metadata=='NA']=NA
#metadata=na.omit(metadata)
df=na.omit(df)
df=df[which(df$Place %in% metadata$Place),]

#Fossil Age Cal BP
Age_LGP=read.csv2("ModeleAge_LGP.txt",sep=',',dec='.')
Age_LDB=read.csv2("ModeleAge_LDB.txt",sep=',',dec='.')
Age_Furamoos=read.csv2("ModeleAge_Furamoosv2.csv",sep=',',dec='.')
Age_Echets=read.csv2("ModeleAge_Echets.txt",sep=',',dec='.')
Age_Eifel=read.csv2("ModeleAge_Eifel.csv")

#A Moduler !!!
crest_df[,1]=Age_LDB$X2

full_df <- merge(metadata, df, on=c('ID', 'place', 'Dec Lat', 'Dec Lon'), all=TRUE)
metadata_id    <- c(1:2,5:6)
coordinates_id <- 3:4
climate_id     <- c(7:11)
climate_names  <- colnames(full_df)[climate_id]
taxa_id        <- 12:(1150-18)
taxa_names     <- colnames(full_df)[taxa_id]
#Contraindre lat
#full_df <- full_df[full_df[, 'Dec Lat'] > 40, ]
#Biome selection !!!
print("Attention BIOME")
full_df <- full_df[full_df[, 'Biome'] == biome[b], ]

#Secondly
distributions <- full_df[FALSE, c(1,2, coordinates_id, climate_id)]
#TaxaSelect=data.frame(matrix(NA,ncol=1,nrow=ncol(full_df)))
#TaxaSelect[1,1]="ID"
#n=2
for(tax in taxa_names) {
    w <- which(full_df[, tax] > 0)
    #if (length(w)!=0)
    if (tax  %in% names(crest_df)&((length(w)!=0)))
    {{distributions <- rbind(distributions,
                            cbind( 'taxonid' = rep(tax, length(w)),
                                   'ProxyName' = rep(tax, length(w)),
                                   #'weight' = full_df[w, tax],
                                   full_df[w, coordinates_id],
                                   full_df[w, climate_id]
                                 )
                           )
    #TaxaSelect[n,1]=tax
    }}}    
colnames(distributions)[1:4] <- c('taxonid', 'ProxyName', 'longitude', 'latitude')
head(distributions)
climate_space <- full_df[, c(coordinates_id, climate_id)]
colnames(climate_space)[1:2] <- c('longitude', 'latitude')
head(climate_space)
crest_df=crest_df[,c('X2',unique(distributions$ProxyName))]
crest_df=na.omit(crest_df)

#Moduler !!!
#write.csv2(distributions, file='./LGP/cliametspece_LGP/',biomeSelect,'.csv')
#write.csv2(distributions, file='./LDB/distributions_WAST_LDB.csv')
#write.csv2(climate_space,file='./LDB/climate_space_WAST_LDB.csv')

#TaxaSelect=na.omit(TaxaSelect)
#TaxaSelect=full_df[which(names(full_df) %in% TaxaSelect),]
#TaxaSelect1=full_df[,TaxaSelect$matrix.NA..ncol...1..nrow...ncol.full_df..]
#full_df_End=full_df[, c(taxa_id)][which(names(full_df[, c(1, taxa_id)]) %in% distributions$ProxyName),]
rcnstrctn <- crest.set_modern_data(distributions,weight=FALSE,minGridCells=15,df=crest_df, climate_space=climate_space, climate = climate_names[seq(1,5)])
#rcnstrctn <- crest.set_modern_data( distributions,
                                    #weight=FALSE,
                                    #df=full_df[, c(1, taxa_id)],
                                    #climate_space=climate_space,
                                    #climate = climate_names[1:5])
#Pour rcnstrct
rcnstrctn <- crest.calibrate(rcnstrctn,geoWeighting=TRUE,climateSpaceWeighting=FALSE,shape=c('normal','lognormal','normal','normal','normal'),bin_width =c(10,100,10,10,100),npoints=1000,verbose=TRUE)
#Accroitre npoints
#Taxa select before rcnstrct
TaxaSelect=data.frame(matrix(NA,ncol=1,nrow=length(rcnstrctn$inputs$taxa.name)))
for (pol in 1:length(rcnstrctn$inputs$taxa.name))
{if((anyNA(list(rcnstrctn$modelling$pdfs[pol]),recursive=TRUE)==TRUE))
{TaxaSelect[pol,1]=rcnstrctn$inputs$taxa.name[pol]}
if (nrow(unique(data.frame(rcnstrctn$modelling$distributions[pol])))<3)
{print(rcnstrctn$inputs$taxa.name[pol])
TaxaSelect[pol,1]=rcnstrctn$inputs$taxa.name[pol]}}
TaxaSelect=na.omit(TaxaSelect)
#rcnstrctn <- excludeTaxa(rcnstrctn,names(apply(crest_df, 2, function(x) return(sum(x>0))))[apply(crest_df, 2, function(x) return(sum(x>0)))>60],climate=rcnstrctn$parameters$climate)
rcnstrctn <- excludeTaxa(rcnstrctn,taxa=TaxaSelect[,1], climate=rcnstrctn$parameters$climate)

#rcnstrctn <- crest.calibrate(rcnstrctn,geoWeighting=FALSE,climateSpaceWeighting=FALSE,shape=c('normal','lognormal','normal'),bin_width =c(20,100,10),npoints=500,verbose=TRUE)
rcnstrctn <- crest.reconstruct(rcnstrctn,
                                presenceThreshold = 0,
                                taxWeight = "normalisation",
                                uncertainties = c(0.5,0.95),
                                verbose=TRUE)
#Moduler !!!
if (b==1)
{dataTEFO=data.frame(rcnstrctn$reconstructions$T_ann$optima,rcnstrctn$reconstructions$T_ann$uncertainties,rcnstrctn$reconstructions$P_ann$optima,rcnstrctn$reconstructions$P_ann$uncertainties,rcnstrctn$reconstructions$MTCM$optima,rcnstrctn$reconstructions$MTCM$uncertainties,rcnstrctn$reconstructions$MTWM$optima,rcnstrctn$reconstructions$MTWM$uncertainties,rcnstrctn$reconstructions$GA$optima,rcnstrctn$reconstructions$GA$uncertainties)}
if (b==2)
{dataWTFO=data.frame(rcnstrctn$reconstructions$T_ann$optima,rcnstrctn$reconstructions$T_ann$uncertainties,rcnstrctn$reconstructions$P_ann$optima,rcnstrctn$reconstructions$P_ann$uncertainties,rcnstrctn$reconstructions$MTCM$optima,rcnstrctn$reconstructions$MTCM$uncertainties,rcnstrctn$reconstructions$MTWM$optima,rcnstrctn$reconstructions$MTWM$uncertainties,rcnstrctn$reconstructions$GA$optima,rcnstrctn$reconstructions$GA$uncertainties)}
if (b==3)
{dataBOFO=data.frame(rcnstrctn$reconstructions$T_ann$optima,rcnstrctn$reconstructions$T_ann$uncertainties,rcnstrctn$reconstructions$P_ann$optima,rcnstrctn$reconstructions$P_ann$uncertainties,rcnstrctn$reconstructions$MTCM$optima,rcnstrctn$reconstructions$MTCM$uncertainties,rcnstrctn$reconstructions$MTWM$optima,rcnstrctn$reconstructions$MTWM$uncertainties,rcnstrctn$reconstructions$GA$optima,rcnstrctn$reconstructions$GA$uncertainties)}
if (b==4)
{dataTUND=data.frame(rcnstrctn$reconstructions$T_ann$optima,rcnstrctn$reconstructions$T_ann$uncertainties,rcnstrctn$reconstructions$P_ann$optima,rcnstrctn$reconstructions$P_ann$uncertainties,rcnstrctn$reconstructions$MTCM$optima,rcnstrctn$reconstructions$MTCM$uncertainties,rcnstrctn$reconstructions$MTWM$optima,rcnstrctn$reconstructions$MTWM$uncertainties,rcnstrctn$reconstructions$GA$optima,rcnstrctn$reconstructions$GA$uncertainties)}
if (b==5)
{dataSTEP=data.frame(rcnstrctn$reconstructions$T_ann$optima,rcnstrctn$reconstructions$T_ann$uncertainties,rcnstrctn$reconstructions$P_ann$optima,rcnstrctn$reconstructions$P_ann$uncertainties,rcnstrctn$reconstructions$MTCM$optima,rcnstrctn$reconstructions$MTCM$uncertainties,rcnstrctn$reconstructions$MTWM$optima,rcnstrctn$reconstructions$MTWM$uncertainties,rcnstrctn$reconstructions$GA$optima,rcnstrctn$reconstructions$GA$uncertainties)}
}

#write.csv2(data,biomeSelect[b])



#Inter-comparison 
dataMegaBiomization=read.csv2("Output_Recon_CREST_LGP.csv")

ggplot(dataMegaBiomization,aes(X/1E3,mean))+
  xlab("Age (kyr)") +
  ylab("Mean Annual Temperature (°C)") +
  geom_line(data=dataTEFOSecond,aes(X/1E3,mean),color="#CAFFCA")+
  geom_line(data=dataWTFOSecond,aes(X/1E3,mean),color='#582900')+
  geom_line(data=dataBOFOSecond,aes(X/1E3,mean),color="#117733")+
  geom_line(data=dataTUNDSecond,aes(X/1E3,mean),color='grey')+
  geom_line(data=dataSTEPSecond,aes(X/1E3,mean),color='bisque')+
  geom_line(data=dataTEFO[0:27,],aes(X2/1E3,mean),color="#CAFFCA")+
  geom_line(data=dataWTFO[0:27,],aes(X2/1E3,mean),color='#582900')+
  geom_line(data=dataBOFO[0:27,],aes(X2/1E3,mean),color="#117733")+
  geom_line(data=dataTUND[0:27,],aes(X2/1E3,mean),color='grey')+
  geom_line(data=dataSTEP[0:27,],aes(X2/1E3,mean),color='bisque')+
  geom_line(color='red',size=2)+
  geom_ribbon(aes(ymin=X0.5_inf,ymax=X0.5_sup, alpha = 0.1))+
  xlim(0,40)+
  ylim(-5,15)+
  theme(axis.text.x = element_text(size=25),axis.text.y = element_text(size=25),panel.background = element_rect(fill = "white"),axis.line = element_line(color = "black",size = 1, linetype = "solid"))
c("#CAFFCA",'#582900',"#117733",'grey',"#E69F00","bisque")



#Globale LOO using LDB
reconstr <- loo(rcnstrctn)
data(reconstr)
lapply(reconstr$reconstructions$P_ann$loo, head)
plot_loo(reconstr,climate='GA',taxanames=c("Poaceae","Pinus","Quercus.robur.type","Betula","Quercus.ilex.type","Brassicaceae"),width=20,bar_width=20,height=20,
         #save=TRUE,
         #filename='LOO_LDB_Globale.png',as.png=TRUE,
         sort ="decr",
         filter = 0.25,
         xlim=c(0,40000),
         col_pos=c('blue'),
         col_neg=c('red'))

plot(rcnstrctn, climate = 'T_ann', simplify=TRUE, uncertainties=c(0.4, 0.6, 0.8))
plot(rcnstrctn, climate = 'P_ann', simplify=TRUE, uncertainties=c(0.4, 0.6, 0.8))

OutBiomizationLGP=read.csv2("OutLGP_Biomisation_1.csv")
OutBiomizationLGP=OutBiomizationLGP[,biome]
OutBiomizationLGP1=OutBiomizationLGP
for (i in 1:nrow(OutBiomizationLGP))
{OutBiomizationLGP[i,]=OutBiomizationLGP[i,]/sum(OutBiomizationLGP1[i,])
if (sum(OutBiomizationLGP1[i,])==0)
{OutBiomizationLGP[i,]=0}}
OutBiomizationLGP=na.omit(OutBiomizationLGP)
write.csv2(OutBiomizationLGP,"OutBiomizationLGP_Normalized.csv")



#%%------------------
#export(rcnstrctn,format='csv',loc = getwd(), as.csv = FALSE, fullPosterior = TRUE, loo = TRUE, weights = FALSE, pdfs = FALSE)

#Plot à discuter
plot("Youpi les plots !!!")
plot_climateSpace(rcnstrctn, save=TRUE, width=6, height=27, add_modern=TRUE)
plot_taxaCharacteristics(rcnstrctn, width = 6, height = 38)
plot(rcnstrctn, save=TRUE)
plot_combinedPDFs(rcnstrctn, save=TRUE, width=6, height=6)
plot_loo(rcnstrctn, save=TRUE, width=6, height=10)

par(mfrow=n2mfrow(length(rcnstrctn$parameters$climate)))
for(clim in rcnstrctn$parameters$climate) {
    COL <- rcnstrctn$reconstructions[[rcnstrctn$parameters$climate[1]]]$optima$optima-full_df[, rcnstrctn$parameters$climate[1]]
    plot(full_df[, clim], rcnstrctn$reconstructions[[clim]]$optima$optima, main=clim, xlab='Observed', ylab='reconstructed', col=ifelse(COL > 3, 'red', 'black'))
    abline(b=1, a=0, col='cornflowerblue')
}

#plot(full_df[, coordinates_id], col=ifelse(COL > 3, 'red', 'black'), asp=1)
#par(mfrow=c(1,1))
#taxa_mean_clim <- rep(NA, length(taxa_names))
#for(i in 1:length(taxa_names)) {
 #   taxa_mean_clim[i] <- sum(full_df[, climate_id[2]] * full_df[, taxa_names[i]] * (full_df[, taxa_names[i]] > 5))
  #  taxa_mean_clim[i] <- taxa_mean_clim[i] / sum(full_df[, taxa_names[i]] * (full_df[, taxa_names[i]] > 5))
#}
#plot(0,0, xlim=c(0, max(full_df[, climate_id[2]])), ylim=c(1,length(taxa_names)), xlab=colnames(full_df)[climate_id[2]])
#idx=1
#for(tax in colnames(full_df[, taxa_names])[order(taxa_mean_clim)]) {
 #   for(i in 1:nrow(full_df)) {
  #      if(full_df[i, tax] > 5) points(full_df[i, climate_id[2]], idx, cex=sqrt(full_df[i, tax]), pch=20, col=idx)
   # }
  #  text(min(full_df[, climate_id[2]])-0.5, idx, tax, adj=c(1, 0.4))
  #  idx = idx+1
#}
#points(taxa_mean_clim[order(taxa_mean_clim)], 1:length(taxa_names), type='l', lwd=2)
#abline(v=range(taxa_mean_clim, na.rm=TRUE), lty=3, lwd=2)
#END
