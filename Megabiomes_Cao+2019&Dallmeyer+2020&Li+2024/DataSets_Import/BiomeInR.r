###Biome reconstruction in R
#read data
variable <- read.csv2("pollen_variable.csv",sep=';',dec=',',row.names=1)
bio <- read.csv2("PFT_Megabiomes_Europe.csv",sep=';',dec=",",row.names=1)
pft <- read.csv2("Taxon_PFT_Europe.csv",sep=';',dec=",",row.names=1)

#Formatage
Name=read.csv2("NameEifel.txt",sep='\t',dec=',')
pollen=read.csv2("AssFEifel.csv",sep=';',dec=',') 
pollen=pollen[,-c(1)]
pollen=pollen*100
colnames(pollen)=Name$Names

##############
#Homogénéisation LDB
for (i in 1:nrow(pollen))
{pollen[i,"Quercus ilex-type"]=pollen[i,"Quercus ilex-type"]+pollen[i,"Quercus robur-type"]
pollen[i,"Asteraceae subf. Asteroideae"]=pollen[i,"Asteraceae subf. Asteroideae"]+pollen[i,"Asteraceae subf. Cichorioideae"]}
colnames(pollen)[colnames(pollen)=="Quercus ilex-type"]="Quercus"
colnames(pollen)[colnames(pollen)=="Ranunculaceae"]="Ranunculus-type"

#Homogénéisation LGP
for (i in 1:nrow(pollen))
{pollen[i,"Quercus ilex-type"]=pollen[i,"Quercus ilex-type"]+pollen[i,"Quercus robur-type"]
pollen[i,"Asteraceae subf. Asteroideae"]=pollen[i,"Asteraceae subf. Asteroideae"]+pollen[i,"Asteraceae subf. Cichorioideae"]}
colnames(pollen)[colnames(pollen)=="Quercus ilex-type"]="Quercus"

#Homogénéisation Furamoos
colnames(pollen)[colnames(pollen)=="Ephedra distachya type"]="Ephedra"
colnames(pollen)[colnames(pollen)=="Euphorbiaceae"]="Euphorbia"
colnames(pollen)[colnames(pollen)=="Ranunculus acris type"]="Ranunculus-type"
colnames(pollen)[colnames(pollen)=="Saxifragaceae"]="Saxifraga"

#Homogénéisation Eifel
colnames(pollen)[colnames(pollen)=="Ranunculaceae"]="Ranunculus-type"

#TEST
setdiff(names(pollen),rownames(pft))
############

NamePol=intersect(rownames(pft),colnames(pollen))
pft=pft[which(rownames(pft) %in% NamePol),] 
pollen=pollen[,which(colnames(pollen) %in% NamePol)]

pollen.no <- nrow(pft)
pollennames <- rownames(pft)

####minus the threhold
for(i in 1:pollen.no){pollen[,pollennames[i]] <- pollen[,pollennames[i]]-0.5}
pollen1 <-pollen[,pollennames]
pollen1[pollen1<0] <- 0
pollen[,pollennames] <- pollen1

####Multiplied by weight - Larix and Pinus
for(i in 1:nrow(variable))
{if (rownames(variable[i,]) %in% pollennames)
{print(rownames(variable[i,]))
pollen[,pollennames[i]] <- pollen[,pollennames[i]]*variable[i,"weight"]}}

####calculate the biome score sample by sample
###set the result matrix
sample.no <- nrow(pollen)
biome.no <- nrow(bio)
biomenames <- row.names(bio)
res <- matrix(0, nrow=sample.no, ncol=biome.no,byrow=TRUE)
res <- data.frame(res)
colnames(res) <- c(biomenames)
####calculation
for(i in 1:sample.no){for(j in 1:biome.no){biome1 <- rownames(bio)[j]
                                           pft1 <- colnames(bio)[bio[j,]==1]
                                           pollen2 <- pollennames[rowSums(pft[,pft1])>0]
                                           res[i,biome1] <- sum(sqrt(pollen[i,pollen2])) 
                                           }
                      print(i)
                      }


for(i in 1:sample.no){res[i,"Best"]<- biomenames[which.max(res[i,biomenames])]
                      res[i,"Best.num"]<- which.max(res[i,biomenames])}
write.csv(res, file="OutModern_MegaBiomization_Eifel.csv")

##END







#######Modern
variable <- read.csv2("pollen_variable.csv",sep=';',dec=',',row.names=1)
bio <- read.csv2("PFT_Megabiomes_Europe.csv",sep=';',dec=",",row.names=1)
pft <- read.csv2("Taxon_PFT_Europe.csv",sep=';',dec=",",row.names=1)
Harmonization <- read.csv2("harmonization_table.txt",sep='\t',dec=",")

#Dataset-All were already normalized
EMPD=read.csv2("EMPD_Normalized-v3_Megabiomes.txt",sep='\t',dec=",")
SiteNameCal=read.csv2("SiteName_ModernDataB.txt",sep='')
Loc=read.csv2("PositionSiteCal.csv",sep=';',dec=",")
Loc[,3]=unique(EMPD[,1])
NameCal=Loc[,3]
Loc=na.omit(Loc)
#Contraindre lat -> Europe
Loc<-Loc[Loc[, 'Latitude'] > 0, ]
Loc<-Loc[Loc[, 'Latitude'] < 90, ]
Loc<-Loc[Loc[, 'Longitude'] > -10, ]
Loc<-Loc[Loc[, 'Longitude'] < 125, ]
EMPD=subset(EMPD,EMPD[,1] %in% Loc[,3])

for (i in 1:nrow(Harmonization))
{if (Harmonization[i,"TaxonName"] %in% unique(EMPD[,2]))
{EMPD[,2][EMPD[,2]==Harmonization[i,"TaxonName"]]=Harmonization[i,"Harmonization"]}}

EMPD=subset(EMPD,EMPD[,2]!="Delete")

EMPD3=EMPD
for(i in 1:(length(EMPD[,3])))
{EMPD[i,3]=(100*EMPD[i,3])/(sum(EMPD3[,3][(EMPD3[,1]==EMPD3[i,1])]))}

#########

NamePol=intersect(rownames(pft),EMPD[,2])
NameCal=Loc[,3]
pollen=EMPD

#TEST
setdiff(unique(pollen$acc_varname),rownames(pft))

pft=pft[which(rownames(pft) %in% NamePol),] 
pollen=subset(pollen, pollen$acc_varname %in% NamePol)

pollen.no <- nrow(pft)
pollennames <- rownames(pft)

####minus the threhold
for(i in 1:pollen.no){pollen$count[pollen$acc_varname==pollennames[i]] <- pollen$count[pollen$acc_varname==pollennames[i]]-0.5}
pollen$count[pollen$count<0] <- 0

####Multiplied by weight - Larix and Pinus
for(i in 1:nrow(variable))
{if (rownames(variable[i,]) %in% pollennames)
{print(rownames(variable[i,]))
pollen$count[pollen$acc_varname==pollennames[i]] <- pollen$count[pollen$acc_varname==pollennames[i]]*variable[i,"weight"]}}

####calculate the biome score sample by sample
###set the result matrix
sample.no <- length(unique(pollen$SampleName))
#info.no <- ncol(pollen)-pollen.no
biome.no <- nrow(bio)
biomenames <- row.names(bio)
res <- matrix(0, nrow=sample.no, ncol=biome.no,byrow=TRUE)
res <- data.frame(res)
#res[,1:info.no] <- pollen[1:info.no]
colnames(res) <- c(biomenames)
####calculation
for(i in 1:sample.no){for(j in 1:biome.no)
{biome1 <- rownames(bio)[j]
pft1 <- colnames(bio)[bio[j,]==1]
pollen2 <- pollennames[rowSums(pft[,pft1])>0]
res[i,biome1] <- sum(sqrt(pollen$count[(pollen$SampleName==NameCal[i])&(pollen$acc_varname %in% pollen2)])) 
}
  print(i)
}


for(i in 1:sample.no){res[i,"Best"]<- biomenames[which.max(res[i,biomenames])]
res[i,"Best.num"]<- which.max(res[i,biomenames])}
write.csv(res, file="OutModern_MegaBiomization_Modern.csv")
write.csv(Loc, file="PositionSiteCal_Output.csv")
