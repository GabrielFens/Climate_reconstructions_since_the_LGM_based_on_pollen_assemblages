###Biome reconstruction in R
#read data
#pollen <- read.csv("pollen_data.csv")
#pollen=read.csv2("pollenEMPD_Foramtting_LastVersion.txt")
#variable <- read.csv2("pollen_variable.csv",sep=';',dec=".",row.names=1)
bio <- read.csv("pft-biome_2.csv",sep=';',dec=",",row.names=1)
pft <- read.csv2("pollen-pft_2.csv",sep=';',dec=",",row.names=1)

Name=read.csv2("NameLGP_2.txt",sep='\t',dec=',')
pollen=read.csv2("AssFLGP_2.csv",sep=';',dec=',') 
pollen=pollen[,-c(1)]
pollen=pollen*100
colnames(pollen)=Name$Names

NamePol=intersect(rownames(pft),colnames(pollen))

#variable=variable[which(row.names(variable) %in% NamePol),] 
pft=pft[which(rownames(pft) %in% NamePol),] 
pollen=pollen[,which(colnames(pollen) %in% NamePol)]

pollen.no <- nrow(pft)
pollennames <- rownames(pft)

####minus the threhold
for(i in 1:pollen.no){pollen[,pollennames[i]] <- pollen[,pollennames[i]]-0.5}
pollen1 <-pollen[,pollennames]
pollen1[pollen1<0] <- 0
pollen[,pollennames] <- pollen1

####Multiplied by weight
#for(i in 1:pollen.no){pollen[,pollennames[i]] <- pollen[,pollennames[i]]*variable[i,"weight"]}

####calculate the biome score sample by sample
###set the result matrix
sample.no <- nrow(pollen)
#info.no <- ncol(pollen)-pollen.no
biome.no <- nrow(bio)
biomenames <- row.names(bio)
res <- matrix(0, nrow=sample.no, ncol=biome.no,byrow=TRUE)
res <- data.frame(res)
#res[,1:info.no] <- pollen[1:info.no]
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
write.csv(res, file="OutModern_Biomization4_LDB.csv")

##END

#######Modern
bio <- read.csv("pft-biome_2.csv",sep=';',dec=",",row.names=1)
pft <- read.csv2("pollen-pft_2.csv",sep=';',dec=",",row.names=1)
#Dataset-All were already normalized
#pollen=read.csv2("AssFLDB_Verif.csv",sep=';',dec=',') 
#####
EMPD=read.csv2("EMPD_Normalized-v3.txt",sep='\t',dec=",")
SiteNameCal=read.csv2("SiteName_ModernDataB.txt",sep='')
Loc=read.csv2("PositionSiteCal.csv",sep=';',dec=".")
Loc[,3]=unique(EMPD[,1])
NameCal=Loc[,3]
Loc=na.omit(Loc)
#Contraindre lat -> Europe
Loc<-Loc[Loc[, 'Latitude'] > 0, ]
Loc<-Loc[Loc[, 'Latitude'] < 90, ]
Loc<-Loc[Loc[, 'Longitude'] > -10, ]
Loc<-Loc[Loc[, 'Longitude'] < 125, ]
EMPD=subset(EMPD,EMPD[,1] %in% Loc[,3])

#####

NamePol=intersect(rownames(pft),EMPD[,2])
NameCal=Loc[,3]
pollen=EMPD

#variable=variable[which(row.names(variable) %in% NamePol),] 
pft=pft[which(rownames(pft) %in% NamePol),] 
pollen=subset(pollen, pollen$acc_varname %in% NamePol)

pollen.no <- nrow(pft)
pollennames <- rownames(pft)

####minus the threhold
for(i in 1:pollen.no){pollen$count1[pollen$acc_varname==pollennames[i]] <- pollen$count1[pollen$acc_varname==pollennames[i]]-0.5}
pollen$count1[pollen$count1<0] <- 0

####Multiplied by weight
#for(i in 1:pollen.no){pollen[,pollennames[i]] <- pollen[,pollennames[i]]*variable[i,"weight"]}

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
res[i,biome1] <- sum(sqrt(pollen$count1[(pollen$SampleName==NameCal[i])&(pollen$acc_varname %in% pollen2)])) 
}
  print(i)
}


for(i in 1:sample.no){res[i,"Best"]<- biomenames[which.max(res[i,biomenames])]
res[i,"Best.num"]<- which.max(res[i,biomenames])}
write.csv(res, file="OutModern_Biomization4_Modern.csv")
write.csv(Loc, file="PositionSiteCal.csv")




################Comments
colnames(pollen)[5]="Acer campestre-type"
colnames(pollen)[6]="Caryophyllaceae (periporate excl. Paronychioideae)"
colnames(pollen)[7]="Castanea sativa" 
colnames(pollen)[8]="Chenopodiaceae/Amaranthaceae"  
colnames(pollen)[9]="Hippophae rhamnoides"  
colnames(pollen)[10]="Sanguisorba minor-type" 

NamePol=intersect(rownames(pft),names(pollen))
NameCal=Loc$Name
ColNames=c(NamePol)

pollen=data.frame(matrix(NA,ncol=length(NamePol),nrow=length(Loc$Name)))
dimnames(pollen)=list(c(Loc$Name),c(ColNames))
for (a in Loc$Name)
{for (j in NamePol)
{if (is_empty(EMPD[,3][(EMPD[,1]==a)&(EMPD[,2]==j)])==FALSE)
{pollen[a,j]=sum(EMPD[,3][(EMPD[,1]==a)&(EMPD[,2]==j)])}}}
pollen[is.na(pollen)==TRUE]=0

#pollen=data.frame(NameCal,Loc$Longitude,Loc$Latitude,pollen)
write.csv2(pollen,"pollenEMPD_Formatting_LastVersion.csv")
