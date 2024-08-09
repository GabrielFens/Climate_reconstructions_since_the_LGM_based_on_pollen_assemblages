####################### GF Biomisation modeling - mars 2024
#Biomisation-PFTsPrentice
library(ggplot2)
library(plotly)
library(dplyr)
library(sf)
library(rnaturalearth)
library(stringr)

#EMPD2 calibration (Chevalier et al., 2019) and its site names 
#EMPD<-read.csv2("EMPD2_20202021.txt",sep='\t',dec='.')
#PA=read.csv2("Taxpftpi2.csv",sep=';',dec=',')
#BPFT=read.csv2("biopftpi2.csv",sep=';',dec='.')
SiteNameCal=read.csv2("SiteName_ModernDataB.txt",sep='')
NameCal=SiteNameCal$Name
#Initialization of output results: the biome returned, dominated affinity score and score dispersion over PFT
Output=data.frame(matrix(NA,ncol=3,nrow=nrow(SiteNameCal)))
dimnames(Output)=list(c(SiteNameCal$Name),c("Biome","Score","Std"))
#rownames(Output)=make.names(AgesCal$X2, unique=TRUE)

#Datasets importation
EMPD=read.csv2("DatasetModernCount_EMPD2_Biomization.txt",sep='\t',dec=",")
EMPD2=read.csv2("Plantago_lanceolata-type_Add.txt",sep='\t',dec=",")xz
#Plantago lanceolata
for(i in 1:(length(EMPD2[,3])))
{EMPD[,2][(EMPD[,1]==EMPD2[i,1])&(EMPD[,3]==EMPD2[i,3])&(EMPD[,2]=="Plantago")]=EMPD2[1,2]}
write.csv2(EMPD,"EMPD_Count.csv")
EMPD3=EMPD
#Normalization of calibration datasets
for(i in 1:(length(EMPD[,3])))
{EMPD[i,3]=(100*EMPD[i,3])/(sum(EMPD3[,3][(EMPD3[,1]==EMPD3[i,1])]))}
write.csv2(EMPD,"EMPD_Normalized.csv")

#Premier classification
PA=read.csv2("IsoplotR-32.txt",sep='\t',dec=',')
rownames(PA)=PA[,1]
PA=PA[,-1]
PA1=PA
BPFT=read.csv2("biompft3.csv",sep=';',dec=",")
Name_Biome=BPFT[,1]
row.names(BPFT)=Name_Biome
BPFT=BPFT[,-1]
BPFT1=BPFT

###End 

#for (a in NameCal)
#{PA=PA1
#for (j in 1:nrow(PA))
#  {for (k in 2:ncol(PA))
#  {if ((PA[j,k]!=0)&(length(EMPD[,3][(EMPD[,1]==a)&(EMPD[,2]==row.names(PA[j,]))])==0))
#  {PA[j,k]=0}
#  if ((PA[j,k]!=0)&(length(EMPD[,3][(EMPD[,1]==a)&(EMPD[,2]==row.names(PA[j,]))])!=0))
#  {if(PA[j,k]<sum(EMPD[,3][(EMPD[,1]==a)&(EMPD[,2]==row.names(PA[j,]))]))
#  {PA[j,k]=sqrt(sum(EMPD[,3][(EMPD[,1]==a)&(EMPD[,2]==row.names(PA[j,]))])-PA[j,k])}}
#  if ((PA[j,k]!=0)&(length(EMPD[,3][(EMPD[,1]==a)&(EMPD[,2]==row.names(PA[j,]))])!=0))
#  {if(PA[j,k]>sum(EMPD[,3][(EMPD[,1]==a)&(EMPD[,2]==row.names(PA[j,]))]))
#  {PA[j,k]=0}}}}
#BPFT=read.csv2("biopftpi_EMPD.txt",sep='\t',dec=",")
#Name_Biome=BPFT[,1]
#BPFT=BPFT[-c(1)]
#PA=PA[-c(1)]
#row.names(BPFT)=Name_Biome
#BPFT=BPFT[,-1]
#Second data.frame about PFT and biome relationship
#for(i in colnames(PA))
#{BPFT[BPFT==i]=sum(PA[i])}
#BiomeIndexResult=data.frame(matrix(NA,ncol=1,nrow=length(Name_Biome)))
#row.names(BiomeIndexResult)=Name_Biome
#for(i in 1:(nrow(BiomeIndexResult)))
#{BiomeIndexResult[i,]<-sum(as.numeric(BPFT[i,-c(1)]),na.rm=TRUE)}
#if (length(max(BiomeIndexResult,na.rm=TRUE)!=1))
#{NameScore=NameScore[1]
#Max=Max[1]
#Sd=Sd[1]}
#Return max of affinity score and statistic parameters
#Output[n,]=c(NameScore,Max,Sd)
#n=n+1}

#2.Fossil site Biomization (csv normalize)
#Initialization of output results: the biome returned, dominated affinity score and score dispersion over PFT
j=2
EMPD2=read.csv2("EMPDInt.txt",sep='\t',dec=",")
EMPD_LDB=read.csv2("DatasetModerneCount_LDB.txt",sep='\t',dec=",")
EMPD_LGP=read.csv2("DatasetModerneCount_LGP.txt",sep='\t',dec=",")
#Saxifraga and Carpinus betulus
for(i in 1:(length(EMPD2[,3])))
{EMPD_LDB[,2][(EMPD_LDB[,1]==EMPD2[i,1])&(EMPD_LDB[,3]==EMPD2[i,3])&(EMPD_LDB[,2]=="Saxifragaceae")]=EMPD2[1,2]}
write.csv2(EMPD_LDB,"EMPD_Count_LDB.csv")
for(i in 1:(length(EMPD2[,3])))
{EMPD_LGP[,2][(EMPD_LGP[,1]==EMPD2[i,1])&(EMPD_LGP[,3]==EMPD2[i,3])&(EMPD_LGP[,2]=="Saxifragaceae")]=EMPD2[1,2]
EMPD_LGP[,2][(EMPD_LGP[,1]==EMPD2[i,1])&(EMPD_LGP[,3]==EMPD2[i,3])&(EMPD_LGP[,2]=="Carpinus")]=EMPD2[2,2]}
write.csv2(EMPD_LGP,"EMPD_Count_LGP.csv")


PA=read.csv2("Taxpftpi_Class1.csv",sep=';',dec=",")
rownames(PA)=PA[,1]
PA=PA[,-1]
PA1=PA
BPFT=read.csv2("biopftpi_Class1.csv",sep=';',dec=".")
Name_Biome=BPFT[,1]
row.names(BPFT)=Name_Biome
BPFT=BPFT[,-1]
BPFT1=BPFT


#Begin-Modern
#1.Biomization of Modern spectrums  
#First data.frame about Taxons and PFT relationship
EMPD=read.csv2("EMPD_Normalized-v3.csv",sep=';',dec=",")
#EMPD=EMPD[,-c(1)]
SiteNameCal=read.csv2("SiteName_ModernDataB.txt",sep='')
NameCal=SiteNameCal$Name
namefile="OutModern_Biomization2"

#Begin-Fossil
#Dataset-All were already normalized
Name=read.csv2("NameLDB.txt",sep='\t',dec=',')
#AssPol=read.csv2("AssFFuramoos_v3.csv",sep=';',dec=',') 
AssPol=read.csv2("AssFLDB.csv",sep=';',dec=',') 
AssPol=AssPol[,-c(1)]
AssPol=AssPol*100
colnames(AssPol)=Name$Names
AgesCal=read.csv2("ModeleAge_LDB.txt",sep=',',dec='.')
Ages=AgesCal$X2
#AgesCal=data.frame(matrix(NA,ncol=1,nrow=nrow(AssPol)))
#Ages=AgesCal$matrix.NA..ncol...1..nrow...nrow.AssPol..
namefile="OutLDB_Biomisation_2"

EuropeBiomeIndexModern<-function(EMPD,NameCal,namefile) {
library(tidyverse)
#Seconde classification
PA=read.csv2("Taxpftpi_Class1.csv",sep=';',dec=",")
rownames(PA)=PA[,1]
PA=PA[,-1]
PA1=PA
BPFT=read.csv2("biopftpi_Class1.csv",sep=';',dec=".")
Name_Biome=BPFT[,1]
row.names(BPFT)=Name_Biome
BPFT=BPFT[,-1]
BPFT1=BPFT
#Initialization of output results: the biome returned, dominated affinity score and score dispersion over PFT
Output=data.frame(matrix(NA,ncol=3,nrow=nrow(SiteNameCal)))
dimnames(Output)=list(c(SiteNameCal$Name),c("Biome","Score","Std"))
#Initializes_the_BarProgress
pb <- txtProgressBar(min = 0, 
                       max = length(NameCal),
                       style = 3, 
                       width = 50,
                       char = "=")   
n=1
for (a in NameCal)
{PA=PA1
PA=PA[which(row.names(PA) %in% EMPD[,2][EMPD[,1]==a]),] 
for (i in EMPD[,2][EMPD[,1]==a])
{PA[(row.names(PA)==i)&(PA>0)&(PA>EMPD[,3][(EMPD[,1]==a)&(EMPD[,2]==i)])]=0
PA[(row.names(PA)==i)&(PA>0)&(PA<sum(EMPD[,3][(EMPD[,1]==a)&(EMPD[,2]==i)]))]=sqrt(sum(EMPD[,3][(EMPD[,1]==a)&(EMPD[,2]==i)])-PA[(row.names(PA)==i)&(PA>0)&(PA<sum(EMPD[,3][(EMPD[,1]==a)&(EMPD[,2]==i)]))])}
BPFT=BPFT1
  
ScorePFT=data.frame(matrix(NA,nrow=1,ncol=ncol(PA)))
colnames(ScorePFT)=colnames(BPFT)
BPFT=BPFT1
for(i in colnames(PA))
{ScorePFT[colnames(ScorePFT)==i]=sum(PA[i])}
  
for (j in rownames(BPFT))
{B=BPFT[j,]
for(i in 1:ncol(B))
{if ((BPFT[j,i]!='')&(BPFT[j,i]!="+")&(BPFT[j,i]!="-"))
{if ((BPFT[j,i]==names(ScorePFT[i])))
{BPFT[j,i]=as.numeric(ScorePFT[,i])}}}}

for(j in rownames(BPFT))
{B=BPFT[j,]
for (i in 1:ncol(B))
{if ((BPFT[j,i]!='')&(BPFT[j,i]!="+")&(BPFT[j,i]!="-"))
{if ((BPFT[j,i]!=names(ScorePFT[i]))&(length(BPFT[j,BPFT[j,i]])!=0))
{BPFT[j,BPFT[j,i]]=as.numeric(BPFT[j,BPFT[j,i]])+as.numeric(ScorePFT[,colnames(B[i])])
BPFT[j,colnames(B[i])]=''}}}}
  
BPFT2=BPFT
BPFT2[BPFT2=='+']=''
BPFT2[BPFT2=='-']=''
BPFT3=BPFT2
for(i in colnames(PA))
{BPFT2[BPFT2==i]=sum(ScorePFT[i])}
  
BPFT[BPFT=='']=NA
BPFT3[BPFT3=='']=NA
  
SumBiome=data.frame(matrix(NA,ncol=1,nrow=nrow(BPFT)))
for(i in 1:(nrow(SumBiome)))
{SumBiome[i,]<-sum(as.numeric(BPFT2[i,]),na.rm=TRUE)}
  
for(i in colnames(PA))
{BPFT2[BPFT2==i]=sum(ScorePFT[i])}
BPFT[BPFT=='']=NA
BPFT3[BPFT3=='']=NA
  
BPFT4=BPFT
for(i in 1:ncol(BPFT))
{for(j in 1:nrow(BPFT))
{y=j-1
z=j+1
if (is.na(BPFT4[j,i])==FALSE)
{if((BPFT4[j,i]=="+")&(sum(as.numeric(BPFT2[z,]),na.rm=TRUE)==max(SumBiome)))
{BPFT[z,i]=BPFT3[rownames(na.omit(BPFT3[i]))[nrow(na.omit(BPFT3[i]))],i]
BPFT[j,i]=0}
if((BPFT4[j,i]=="-")&(sum(as.numeric(BPFT2[y,]),na.rm=TRUE)==max(SumBiome)))
{BPFT[y,i]=BPFT3[rownames(na.omit(BPFT3[i]))[nrow(na.omit(BPFT3[i]))],i]
BPFT[j,i]=0}
}}}
BPFT[(BPFT=='+')|(BPFT=='-')]=NA
  
BiomeIndexResult=data.frame(matrix(NA,ncol=1,nrow=length(Name_Biome)))
row.names(BiomeIndexResult)=Name_Biome
for(i in 1:(nrow(BiomeIndexResult)))
{BiomeIndexResult[i,]<-sum(as.numeric(BPFT[i,]),na.rm=TRUE)}
NameScore=Name_Biome[which(BiomeIndexResult==BiomeIndexResult[BiomeIndexResult==max(BiomeIndexResult,na.rm=TRUE)])]
Max=max(BiomeIndexResult,na.rm=TRUE)
Sd=sd(BiomeIndexResult$matrix.NA..ncol...1..nrow...length.Name_Biome..,na.rm=TRUE)
if (length(max(BiomeIndexResult,na.rm=TRUE)!=1))
{NameScore=NameScore[1]
Max=Max[1]
Sd=Sd[1]}
#Return max of affinity score and statistic parameters
Output[n,]=c(NameScore,Max,Sd)
n=n+1
Sys.sleep(0.1)
setTxtProgressBar(pb, n)
if(n==length(NameCal)/2){print("Eso eso que")}
}
nameFile=str_c("./",namefile,".csv") 
return(write.csv2(Output,nameFile))}

EuropeBiomeIndexFossil<-function(Name,AssPol,Ages,namefile){
PA=read.csv2("Taxpftpi_Class1.csv",sep=';',dec=",")
rownames(PA)=PA[,1]
PA=PA[,-1]
PA1=PA
BPFT=read.csv2("biopftpi_Class1.csv",sep=';',dec=".")
Name_Biome=BPFT[,1]
row.names(BPFT)=Name_Biome
BPFT=BPFT[,-1]
BPFT1=BPFT
Output=data.frame(matrix(NA,ncol=nrow(BPFT),nrow=nrow(AssPol)))
colnames(Output)=rownames(BPFT)
rownames(Output)=make.names(Ages, unique=TRUE)
n=1
#Initializes_the_BarProgress
pb <- txtProgressBar(min = 0, 
                     max = nrow(AssPol),
                     style = 3, 
                     width = 50,
                     char = "=")   
for (a in 1:nrow(AssPol))
{if(n==1){print("Fasten your seatbelts!")}
PA=PA1
for(i in Name$Names)
{PA=PA[which(row.names(PA) %in% Name$Names),] 
PA[(row.names(PA)==i)&(PA>0)&(PA>AssPol[a,i])]=0
PA[(row.names(PA)==i)&(PA>0)&(PA<AssPol[a,i])]=sqrt(AssPol[a,i]-PA[(row.names(PA)==i)&(PA>0)&(PA<AssPol[a,i])])}
ScorePFT=data.frame(matrix(NA,nrow=1,ncol=ncol(PA)))
colnames(ScorePFT)=colnames(BPFT)
BPFT=BPFT1
for(i in colnames(PA))
{ScorePFT[colnames(ScorePFT)==i]=sum(PA[i])}

for (j in rownames(BPFT))
{B=BPFT[j,]
for(i in 1:ncol(B))
{if ((BPFT[j,i]!='')&(BPFT[j,i]!="+")&(BPFT[j,i]!="-"))
{if ((BPFT[j,i]==names(ScorePFT[i])))
{BPFT[j,i]=as.numeric(ScorePFT[,i])}}}}

for(j in rownames(BPFT))
{B=BPFT[j,]
for (i in 1:ncol(B))
{if ((BPFT[j,i]!='')&(BPFT[j,i]!="+")&(BPFT[j,i]!="-"))
{if ((BPFT[j,i]!=names(ScorePFT[i]))&(length(BPFT[j,BPFT[j,i]])!=0))
{BPFT[j,BPFT[j,i]]=as.numeric(BPFT[j,BPFT[j,i]])+as.numeric(ScorePFT[,colnames(B[i])])
BPFT[j,colnames(B[i])]=''}}}}

BPFT2=BPFT
BPFT2[BPFT2=='+']=''
BPFT2[BPFT2=='-']=''
BPFT3=BPFT2
BPFT2[BPFT2=='']=NA
SumBiome=data.frame(matrix(NA,ncol=1,nrow=nrow(BPFT)))
for(i in 1:(nrow(SumBiome)))
{SumBiome[i,]<-sum(as.numeric(BPFT2[i,]),na.rm=TRUE)}

for(i in colnames(PA))
{BPFT2[BPFT2==i]=sum(ScorePFT[i])}
BPFT[BPFT=='']=NA
BPFT3[BPFT3=='']=NA

BPFT4=BPFT
for(i in 1:ncol(BPFT))
{for(j in 1:nrow(BPFT))
{y=j-1
z=j+1
if (is.na(BPFT4[j,i])==FALSE)
{if((BPFT4[j,i]=="+")&(sum(as.numeric(BPFT2[z,]),na.rm=TRUE)==max(SumBiome)))
{BPFT[z,i]=BPFT3[rownames(na.omit(BPFT3[i]))[nrow(na.omit(BPFT3[i]))],i]
BPFT[j,i]=0}
if((BPFT4[j,i]=="-")&(sum(as.numeric(BPFT2[y,]),na.rm=TRUE)==max(SumBiome)))
{BPFT[y,i]=BPFT3[rownames(na.omit(BPFT3[i]))[nrow(na.omit(BPFT3[i]))],i]
BPFT[j,i]=0}
}}}
BPFT[(BPFT=='+')|(BPFT=='-')]=NA

BiomeIndexResult=data.frame(matrix(NA,ncol=1,nrow=length(Name_Biome)))
row.names(BiomeIndexResult)=Name_Biome
for(i in 1:(nrow(BiomeIndexResult)))
{BiomeIndexResult[i,]<-sum(as.numeric(BPFT[i,]),na.rm=TRUE)
Output[n,i]=BiomeIndexResult[i,]}

#Return max of affinity score and statistic parameters
n=n+1
Sys.sleep(0.1)
setTxtProgressBar(pb, n)
if(n==nrow(AssPol)/2){print("Eso eso que")}
close(pb) 
}
nameFile=str_c("./",namefile,".csv") 
return(write.csv2(Output,nameFile))}
#END









-------------
NameScore=Name_Biome[which(BiomeIndexResult==BiomeIndexResult[BiomeIndexResult==max(BiomeIndexResult,na.rm=TRUE)])]
Max=max(BiomeIndexResult,na.rm=TRUE)
Sd=sd(BiomeIndexResult$matrix.NA..ncol...1..nrow...length.Name_Biome..,na.rm=TRUE)
if (length(max(BiomeIndexResult,na.rm=TRUE)!=1))
{NameScore=NameScore[1]
Max=Max[1]
Sd=Sd[1]}
Output[n,]=c(AgesCal$X2[n],NameScore,Max,Sd)
n=n+1}
write.csv2(Output,nameFile)}
###End 

#for (j in 1:nrow(PA))
#{for (k in 2:ncol(PA))
#{if ((PA[j,k]!=0)&(AssPol[a,row.names(PA[j,])])==0)
#{PA[j,k]=0}
#if ((PA[j,k]!=0)&(AssPol[a,row.names(PA[j,])])!=0)
#{if(PA[j,k]<AssPol[a,row.names(PA[j,])])
#{PA[j,k]=sqrt(AssPol[a,row.names(PA[j,])]-PA[j,k])}}
#if(PA[j,k]>AssPol[a,row.names(PA[j,])])
#{PA[j,k]=0}}}
#BPFT=read.csv2("biopftpi_EMPD.txt",sep='\t',dec=",")
#BPFT=BPFT1
#BPFT=BPFT[-c(1)]
#PA=PA[-c(1)]
#Second data.frame about PFT and biome relationship
#for(i in colnames(PA))
#{BPFT[BPFT==i]=sum(PA[i])}
#BiomeIndexResult=data.frame(matrix(NA,ncol=1,nrow=length(Name_Biome)))
#row.names(BiomeIndexResult)=Name_Biome
#for(i in 1:(nrow(BiomeIndexResult)))
#{BiomeIndexResult[i,]<-sum(as.numeric(BPFT[i,-c(1)]),na.rm=TRUE)}
#Return max of affinity score and statistic parameters
#Max=BiomeIndexResult[BiomeIndexResult==max(BiomeIndexResult,na.rm=TRUE)]
#Output[n,]=c(Name_Biome[which(BiomeIndexResult==Max)],max(BiomeIndexResult,na.rm=TRUE),sd(BiomeIndexResult$matrix.NA..ncol...1..nrow...length.Name_Biome..,na.rm=TRUE))
#n=n+1}
#write.csv2(Output,"OutLDB_Biomisation_1.csv")

###############
#Biome foosile representation
#OutBiomizationFuramoos_1=read.csv2("OutFuramoos_Biomisation_1.csv")
OutBiomizationFuramoos_2=read.csv2("OutFuramoos_Biomisation_2_v3.csv")
#OutBiomizationEifel_1=read.csv2("OutEifel_Biomisation_1.csv")
OutBiomizationEifel_2=read.csv2("OutEifel_Biomisation_2.csv")
#OutBiomizationLGP_1=read.csv2("OutLGP_Biomisation_1.csv")
OutBiomizationLGP_2=read.csv2("OutLGP_Biomisation_2.csv")
#OutBiomizationEchets_1=read.csv2("OutEchets_Biomisation_1.csv")
OutBiomizationEchets_2=read.csv2("OutEchets_Biomisation_2.csv")
#OutBiomizationLDB_1=read.csv2("OutLDB_Biomisation_1.csv")
OutBiomizationLDB_2=read.csv2("OutLDB_Biomisation_2.csv")
Age_LGP=read.csv2("ModeleAge_LGP.txt",sep=',',dec='.')
Age_LDB=read.csv2("ModeleAge_LDB.txt",sep=',',dec='.')
Age_Furamoos=read.csv2("ModeleAge_Furamoos.csv",sep=',',dec='.')
Age_Echets=read.csv2("ModeleAge_Echets.txt",sep=',',dec='.')
Age_Eifel=read.csv2("ModeleAge_Eifel.csv")

Age=Age_LDB$X2
OutBiomization=OutBiomizationLDB_2[,-c(1)]

#Diagramme Cumulatif/Empilé
time=rep(Age,each=ncol(OutBiomization))
Biome=rep(names(OutBiomization),times=length(Age))
OutBiomization1=OutBiomization
for (i in 1:nrow(OutBiomization))
{OutBiomization[i,]=OutBiomization[i,]/sum(OutBiomization1[i,],na.rm=TRUE)}
table=as.vector(t(OutBiomization))
data=data.frame(time,Biome,table)

library(MBCbook)
library(plot.matrix)
Max=data.frame(matrix(NA,ncol=1,nrow=nrow(OutBiomization)))
for (i in 1:nrow(OutBiomization))
{nameMax=names(which.max(OutBiomization1[i,]))
Max[i,]=nameMax[1]}

#UniqueNum=c("CLDE","TAIG","PION","CLMX","COCO","TEDE","COMX","WAMX","XERO","TUND","COST","WAST","CODE","HODE","AQUA","ANTH")
#Num=seq(1,length(UniqueNum),1)
#for (i in Num)
#{Max[Max==UniqueNum[i]]=i}

Nrow=rep(c(1),times=nrow(Max))
Ncol=seq(1,nrow(Max))
dataMax=data.frame(Nrow,Ncol,Max)

library(ggplot2)
library(dplyr)
ggplot(data,aes(time/1E3,table,fill=Biome))+
  geom_area()+
  scale_fill_manual(values=c("black","blue", 'aquamarine',"#332288","#117733",'#EE82EE',"#CAFFCA",'powderblue',"grey","bisque","#CC79A7",'darkorchid',"#E69F00",'#65B2FF','coral','#582900'))+
  theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),fontsize='white',panel.background = element_rect(fill = "white"))
ggplot(data2,aes(Age/1E3,COST))+
  geom_line()+
  theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),fontsize='white',panel.background = element_rect(fill = "white"))

library(ggplot2)
ggplot(Max, aes(x = Ncol, y = Nrow, fill = matrix.NA..ncol...1..nrow...nrow.OutBiomization.. )) +
  geom_raster() +
  guides(fill=guide_legend(title="Biome")) +
  scale_fill_manual(values=c("#332288","#117733",'aquamarine','powderblue',"bisque","#CC79A7",'darkorchid',"#E69F00","coral"), 
                    labels=format(c("CLMX","COCO","COMX","COST","PION",'TAIG',"TEDE","TUND","WAST")))+ 
  theme_classic()


€≠data2=data.frame(Age,OutCOST_Eifel)
OutCOST_Eifel=OutBiomization['COST']  
OutCOST_Furamoos=OutBiomization['COST']          
OutCOST_LGP=OutBiomization['COST']  
OutCOST_Echets=OutBiomization['COST']  
OutCOST_LDB=OutBiomization['COST']  



#############
#Biome space representation
Samples=read.csv2("LonLatSiteCalEMPD2.txt",sep='\t')
OutBiomization2=read.csv2("OutModern_Biomization_2.csv",sep=';',dec='.')
OutBiomization1=read.csv2("OutModern_Biomization_1.csv",sep=';',dec='.')
OutBiomization1$Biome[OutBiomization1$Score==0]='TUND'
OutBiomization2$Biome[OutBiomization2$Score==0]='TUND'
ModernClimate=read.csv2("ModernClimate.csv",sep=';',dec=',')

OutBiomizationFin=OutBiomization2
for (i in 1:nrow(OutBiomizationFin))
{if (OutBiomization2$Score[i]>OutBiomization1$Score[i])
{OutBiomizationFin$Biome[i]=OutBiomization2$Biome[i]
OutBiomizationFin$Score[i]=OutBiomization2$Score[i]}} 
write(c(OutBiomizationFin$X,OutBiomizationFin$Biome,OutBiomizationFin$Score,OutBiomizationFin$Std),'OutBiomization.csv')

#############
Samples=read.csv2("LonLatSiteCalEMPD2.txt",sep='\t')
ModernClimate=read.csv2("ModernClimate.csv",sep=';',dec=',')
OutBiomization=read.csv2("OutModern_Biomization2_v4.csv",sep=';',dec='.')
#UniqueNum=unique(OutBiomization$Biome)
#UniqueNum=c("WAMX","COMX","XERO","COCO","TAIG","TEDE","CLMX","TUND","WAST","CODE","COST","AQUA","ANTH","CLDE")
UniqueNum=c("CLDE","TAIG","PION","CLMX","COCO","TEDE","COMX","WAMX","XERO","TUND","COST","WAST","CODE","HODE","AQUA","ANTH")
Num=seq(1,length(UniqueNum),1)
BiomeNum=OutBiomization$Biome
for (i in Num)
{BiomeNum[BiomeNum==UniqueNum[i]]=i}
BiomeNum=as.numeric(BiomeNum)
BiomeNum=data.frame(BiomeNum)

#barplot(table(BiomeNum),col=c('#65B2FF','#BBCB35',"#E69F00","cyan", '#0020CA',"#55EB49","#CAFFCA","#000065","#FFB3A5","#E7E718","#91283B","grey","blue",'#EE82EE','darkolivegreen'))
barplot(table(BiomeNum),col=c('aquamarine',"#CC79A7","bisque","#332288","#117733",'darkorchid',"#CAFFCA",'#65B2FF','#582900',"#E69F00",'powderblue','coral','#EE82EE',"grey","blue","black"))
boxplot(BiomeNum)
#BiomePlot=data.frame(seq(1,15,1))
#for (i in 1:15)
#{BiomePlot[i,]=length(BiomeNum[BiomeNum==i])/length(BiomeNum)}  
#barplot(BiomePlot/length(BiomeNum),col=c('#65B2FF','#BBCB35',"#E69F00","cyan", '#0020CA',"#55EB49","#CAFFCA","#000065","#FFB3A5","#E7E718","green","grey",'#EE82EE','blue','#F7E718'))

library(ggplot2)
DistLat=data.frame(ModernClimate$T_ann,ModernClimate$P_ann,BiomeNum$BiomeNum)
#InterLat=seq(min(Samples$Latitude,na.rm=TRUE),max(Samples$Longitude,na.rm=TRUE),20)
ggplot(DistLat,aes(y=ModernClimate.T_ann,x=BiomeNum.BiomeNum,fill=as.factor(BiomeNum.BiomeNum))) +
  geom_boxplot(color='grey45',outlier.colour = 'white')+
  scale_fill_manual(values=c('aquamarine',"#CC79A7","bisque","#332288","#117733",'darkorchid',"#CAFFCA",'#65B2FF','#582900',"#E69F00",'powderblue','coral','#EE82EE',"grey","blue","black"))+
  theme_classic()
ggplot(DistLat,aes(y=ModernClimate.P_ann,x=BiomeNum.BiomeNum,fill=as.factor(BiomeNum.BiomeNum))) +
  geom_boxplot(color='grey45',outlier.colour = 'white')+
  scale_fill_manual(values=c('aquamarine',"#CC79A7","bisque","#332288","#117733",'darkorchid',"#CAFFCA",'#65B2FF','#582900',"#E69F00",'powderblue','coral','#EE82EE',"grey","blue","black"))+
  ylim(0,2000)+
  theme_classic()
Position=data.frame(Samples$Latitude,DistLat$BiomeNum.BiomeNum)
Color=c('aquamarine',"#CC79A7","bisque","#332288","#117733",'darkorchid',"#CAFFCA",'#65B2FF','#582900',"#E69F00",'powderblue','coral','#EE82EE',"grey","blue","black")
#for (i in 1:length(UniqueNum))
Position=na.omit(Position)
ggplot(data=Position,aes(Samples.Latitude,colour=as.factor(DistLat.BiomeNum.BiomeNum)),width=1.9) +
    geom_density(size=1)+
    scale_color_manual(values=c('aquamarine',"#CC79A7","bisque","#332288","#117733",'darkorchid',"#CAFFCA",'#65B2FF','#582900',"#E69F00",'powderblue','coral','#EE82EE',"grey","blue","black"))+
    theme_classic()

Loc=data.frame(Samples$Longitude,Samples$Latitude,ModernClimate$T_ann,ModernClimate$P_ann)
Loc=na.omit(Loc)
LatFoss=c(45.90,47.990556,44.83,47.33,50.2073)
LongFoss=c(4.93,9.886944,3.82,6.5038,6.693)
for (i in 1:length(LatFoss))
{Long=Loc$Samples.Longitude[findInterval(LongFoss[i],sort(Loc$Samples.Longitude))]
Lat=Loc$Samples.Latitude[findInterval(LatFoss[i],sort(Loc$Samples.Latitude))]
print(i)
print(Loc$ModernClimate.T_ann[(Loc$Samples.Longitude==Long)&(Loc$Samples.Latitude==Lat)])
print(Loc$ModernClimate.P_ann[(Loc$Samples.Longitude==Long)&(Loc$Samples.Latitude==Lat)])
}
library(ggExtra)
gg=ggplot(DistLat,aes(ModernClimate.T_ann,ModernClimate.P_ann))+
  geom_point()+
  theme_classic()
ggMarginal(gg)

for (i in Num)
{ggplot(DistLat,aes(Samples.Latitude,BiomeNum.BiomeNum))+
boxplot(DistLat$Samples.Latitude[DistLat$BiomeNum.BiomeNum==i])}
hist(BiomeNum,col=c('#65B2FF','#BBCB35',"#E69F00","cyan", '#0020CA',"#55EB49","#CAFFCA","#000065","#FFB3A5","#E7E718","green","grey",'#EE82EE','blue','#F7E718'))
BiomeNum=data.frame(BiomeNum)
Data=data.frame(name=UniqueNum,value=Num)

DataSynthesis=data.frame(Samples$Longitude,Samples$Latitude,BiomeNum)
for (i in 1:(length(InterLat)))
{j=i+1
hist(DataSynthesis$BiomeNum[(DataSynthesis$Samples.Latitude<InterLat[j])&(DataSynthesis$Samples.Longitude>InterLat[i])],col=c('#65B2FF','#BBCB35',"#E69F00","cyan", '#0020CA',"#55EB49","#CAFFCA","#000065","#FFB3A5","#E7E718","green","grey",'#EE82EE','blue','darkolivegreen'))
theme_classic()}

#Homogénéisation avec Hegel
#Me --> Hegel

Data$ExtBiome[Data$ExtBiome==4]=1
Data$ExtBiome[Data$ExtBiome==7]=1
Data$ExtBiome[Data$ExtBiome==8]=2
Data$ExtBiome[Data$ExtBiome==9]=2
Data$ExtBiome[Data$ExtBiome==13]=6
Data$ExtBiome[Data$ExtBiome==14]=7
Data$ExtBiome[Data$ExtBiome==15]=14
Data$ExtBiome[Data$ExtBiome==16]=6
Data$ExtBiome[Data$ExtBiome==17]=6
Data$ExtBiome[Data$ExtBiome==19]=6
Data$ExtBiome[Data$ExtBiome==20]=3
Data$ExtBiome[Data$ExtBiome==22]=3
Data$ExtBiome[Data$ExtBiome==27]=3
Data$ExtBiome[Data$ExtBiome==28]=8
Data$ExtBiome[Data$ExtBiome==30]=8
Data$ExtBiome[Data$ExtBiome==31]=8
Data$ExtBiome[Data$ExtBiome==32]=8
Data$ExtBiome[(Data$Loc.DataSynthesis.BiomeNum==12)]=12
Data$ExtBiome[(Data$Loc.DataSynthesis.BiomeNum==13)]=13
Data$ExtBiome[(Data$Loc.DataSynthesis.BiomeNum==14)]=15


#Hegel --> Me
DataSynthesis$BiomeNum1=DataSynthesis$BiomeNum
for (a in 1:length(DataSynthesis$BiomeNum))
{if (DataSynthesis$BiomeNum1[a]==4)
{DataSynthesis$BiomeNum[a]=2}
if (DataSynthesis$BiomeNum1[a]==3)
{DataSynthesis$BiomeNum[a]=5}
if (DataSynthesis$BiomeNum1[a]==4)
{DataSynthesis$BiomeNum[a]=2}
if (DataSynthesis$BiomeNum1[a]==5)
{DataSynthesis$BiomeNum[a]=3} 
if (DataSynthesis$BiomeNum1[a]==14)
{DataSynthesis$BiomeNum[a]=3} 
if (DataSynthesis$BiomeNum1[a]==7)
{DataSynthesis$BiomeNum[a]=3} 
if (DataSynthesis$BiomeNum1[a]==6)
{DataSynthesis$BiomeNum[a]=4}   
if (DataSynthesis$BiomeNum1[a]==9)
{DataSynthesis$BiomeNum[a]=3}
if (DataSynthesis$BiomeNum1[a]==10)
{DataSynthesis$BiomeNum[a]=6}}
DataSynthesis$BiomeNum[DataSynthesis$BiomeNum>10]=NA

if (DataSynthesis$BiomeNum1[a]==5)
{DataSynthesis$BiomeNum[a]=4}
if (DataSynthesis$BiomeNum1[a]==5)
{DataSynthesis$BiomeNum[a]=4}
if (DataSynthesis$BiomeNum1[a]==6)
{DataSynthesis$BiomeNum[a]=5}
if (DataSynthesis$BiomeNum1[a]==7)
{DataSynthesis$BiomeNum[a]=4}
if (DataSynthesis$BiomeNum1[a]==9)
{DataSynthesis$BiomeNum[a]=6}
if (DataSynthesis$BiomeNum1[a]==10)
{DataSynthesis$BiomeNum[a]=7}
if (DataSynthesis$BiomeNum1[a]==14)
{DataSynthesis$BiomeNum[a]=4}}

#############
library(maptools)
library(mapdata)
library(maps)
library(rnaturalearth)
library(tidyverse)
#Discret
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden")
world_map <- map_data("world")
#world_map <- ne_countries(scale = 50, returnclass = 'sf')
#europe_map <- world_map %>% filter(name %in% europeanUnion)
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  legend.key.size = unit(0.25, "cm"),
  legend.key.width = unit(0.15,"cm"), 
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)
#for (i in 1:length(UniqueNum))
data=subset(DataSynthesis,BiomeNum==7)
data %>%
  rename(x = Samples.Longitude, y = Samples.Latitude) %>%
  ggplot()+
  #geom_map(map=world_map)+
  geom_polygon(data=world_map,aes(x = long, y = lat, group = group),fill='grey90') +
  geom_point(aes(x = x, y = y, color =as.factor(BiomeNum))) +
  scale_color_identity() +
  coord_fixed()+
  scale_color_manual(values=c("black"))+
  #scale_color_manual(values=c('aquamarine',"#CC79A7","bisque","#332288","#117733",'darkorchid',"#CAFFCA",'#65B2FF','#582900',"#E69F00",'powderblue','coral','#EE82EE',"grey","blue","black"))+
  xlim(-60,250)+
  ylim(0,90)+
  xlab("") +
  ylab("")+
  guides(fill = guide_legend(reverse=TRUE))+
  plain
library(tidyverse) 
library(sf)
library(kknn)
library(forcats)
library(tibble)
library(rnaturalearth)

#Discret interpolation
#Partie I
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Republic","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   'Turkey',
                   "Sweden","Russia","Switz","Austria","Ukraine","Belarius","Latvia","Estonia","Belarus","Moldova")
crs_etrs = "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"
knn = 1000 
Grid=ne_download(scale = 10, 
                 type = 'states', 
                 returnclass = 'sf',
                 category = 'cultural')
World_st <- Grid %>%
  filter(admin %in% europeanUnion)%>%
  st_transform(crs_etrs)
#rm(World_st)
# Dissolve internal boundaries 
Europe <- 
  World_st %>%
  group_by(iso_a2) %>% 
  summarize() 
ggplot(Europe) +
  geom_sf(fill="white")

#Partie II
Samples=read.csv2("LonLatSiteCalEMPD2.txt",sep='\t')
ModernClimate=read.csv2("ModernClimate.csv",sep=';',dec=',')
OutBiomization=read.csv2("OutModern_Biomization2_v4.csv",sep=';',dec='.')
DataSynthesis=data.frame(Samples$Longitude,Samples$Latitude,OutBiomization$Biome)
DataSynthesis=na.omit(DataSynthesis)
data_raster=
  DataSynthesis %>%
  st_as_sf(coords = c("Samples.Longitude", "Samples.Latitude"),
           crs = 4326) %>%  
  st_transform(crs_etrs)

#Partie III
geom_buff <- Europe %>%
  st_buffer(dist = 10000)
geom_resp <-
  data_raster %>%
  st_intersection(geom_buff)

#Partie IV
width_in_pixels=100
dx <- ceiling( (st_bbox(geom_buff)["xmax"] - 
                  st_bbox(geom_buff)["xmin"]) / width_in_pixels)
dy=dx
height_in_pixels <- floor( (st_bbox(geom_buff)["ymax"] - 
                              st_bbox(geom_buff)["ymin"])/dy)
grid <- st_make_grid(geom_buff, 
                     cellsize = dx,
                     n = c(width_in_pixels, height_in_pixels),
                     what = "centers")

#DataSynthesis=as_tibble(DataSynthesis)
dialects_input <- geom_resp %>%
  tibble(dialect = .$OutBiomization.Biome, 
         lon = st_coordinates(.)[, 1], 
         lat = st_coordinates(.)[, 2]) %>%
  select(dialect, lon, lat)
dialects_input <- 
  dialects_input %>%
  group_by(dialect) %>% 
  nest() %>% 
  mutate(num = map_int(data, nrow)) %>% 
  arrange(desc(num)) %>% 
  slice(1:8) %>% 
  unnest(cols=c(data)) %>% 
  select(-num)

#Partie V
dialects_thin <-
  dialects_input %>%
  filter(row_number() %% 5 == 1) # Pull out every 5th ro
#Interpolation function 
dialects_output <- data.frame(dialect = as.factor(NA), 
                              lon = st_coordinates(grid)[, 1], 
                              lat = st_coordinates(grid)[, 2])
#run KKNN interpolation function
dialects_kknn <- kknn::kknn(dialect ~ ., 
                            train = dialects_thin, 
                            test = dialects_output, 
                            kernel = "gaussian", 
                            k = knn)
#Extract results to output tibble
dialects_output <-
  dialects_output %>%
  mutate(dialect = fitted(dialects_kknn),
         prob = apply(dialects_kknn$prob, 
                      1, 
                      function(x) max(x)))

#Transform interpolation tibble to sf
dialects_raster <- st_as_sf(dialects_output, 
                            coords = c("lon", "lat"),
                            crs = crs_etrs,
                            remove = F)
geom_rast <- 
  dialects_raster %>%
  st_intersection(Europe)



EuropeMap=map('worldHires', col='gray90', fill=T,xlim=c(-60,200), ylim=c(10,90))
ggplot()+
  geom_polygon(aes(long,lat,group=group),data=EuropeMap)+
  geom_point(aes(x=Samples$Longitude,y=Samples$Latitude,color=BiomeNum),cex=2)
  #legend("bottomright", cex = 0.7, bty = "n")
library(tidyverse) 
library(sf)
library(kknn)
for (i in 1:length(UniqueNum))
{print(i)
data=subset(DataSynthesis,BiomeNum==i)
world_coordinates <- map_data("world") 
ggplot() + 
geom_map( 
  data=world_coordinates, map=world_coordinates, 
  aes(long,lat,map_id=region),
  color = "grey", fill= "white"
)+ 
  geom_point( 
    data = na.omit(data),
    aes(Samples.Longitude,Samples.Latitude,color=as.factor(BiomeNum)), 
    alpha = 1) + 
  #scale_color_manual(values=c('#0030CA',"#55EB49",'#BBCB35','pink',"#E69F00",'gold',"#000065))+
  #scale_color_manual(values=c('#65B2FF','#BBCB35',"#E69F00","cyan",'#0020CA',"#55EB49","#CAFFCA","#000065","#E7E718","#91283B","grey",'#EE82EE','blue'))+
  scale_color_manual(values=c('#65B2FF',"#332288","#B2912f","bisque","#CC79A7","#CAFFCA","#117733",'yellow',"#E69F00",'powderblue','coral',"grey",'#EE82EE',"black","blue",'aquamarine'))+
  #scale_color_manual(values=c('#65B2FF','#BBCB35',"#E69F00","cyan", '#0020CA',"#55EB49","#CAFFCA","#000065","#FFB3A5","#E7E718","#91283B","grey","blue",'#EE82EE','darkolivegreen'))+
  #scale_color_manual(values=c('#65B2FF','#BBCB35',"#E69F00","cyan", '#0020CA',"#55EB49","#CAFFCA","#000065","#FFB3A5","#E7E718","green","grey",'#EE82EE','blue','#F7E718'))+
  theme(panel.background = element_rect(fill = "white",colour = "white",size = 0.5),legend.position="right") 
}
#Continue
library(sf)
library(DT)
library(tidyverse)
library(RPostgres)
library(stars)
library(tmap)
library(variousdata)
library(gstat)
library(raster)
DataMap=data.frame(point=c(DataSynthesis$BiomeNum),Lat=c(DataSynthesis$Samples.Latitude),Long=c(DataSynthesis$Samples.Longitude))
DataMap=na.omit(DataMap)
DataMap=data.frame(point=c(Data$ExtBiome),Lat=c(Data$na.omit.Samples.Latitude.),Long=c(Data$na.omit.Samples.Longitude.))
map=st_as_sf(DataMap, coords = c('Long','Lat'),crs = 4326)
print(map)

#ggplot() + 
#  geom_sf(data = map, aes(fill = point)) + 
#  scale_y_continuous()
data("World")
mercator <- tm_shape(World, projection = st_crs(3395)) + 
  tm_polygons() + 
  tm_layout("Le monde en projection Mercator", inner.margins=c(0,0,.1,0), title.size=.8)
robin <- tm_shape(World, projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0") + 
  tm_polygons() +
  tm_layout(
    "Le monde en projection Robinson",
    inner.margins=c(0,0,.1,0), title.size=.8)
robin+
  tm_shape(map) +
  tm_dots(col = "point", title.size = "Biome discret")

#Interpolation
DataMap=na.omit(DataMap)
example_raster <- raster(crs = crs(map), vals = 0, resolution = c(0.5, 0.5), ext = extent(c(-180, 180, -90, 90))) %>%
  +rasterize(map, .)
ggplot(data =example_raster) +
  geom_raster(aes(x=Long,y=Lat,fill=point))
  # remove propability legend
  scale_alpha_continuous(guide = F) +
  # add color brewer palette and legend titlw
  scale_fill_brewer(palette = "Dark2", 
                    name = "point") +
  theme_map()

current.mode <- tmap_mode("plot")
data(World, metro, rivers)
tm_shape(World) + 
  tm_polygons() + 
  tm_layout("Long lat coordinates (WGS84)", inner.margins=c(0,0,.1,0), title.size=.8)
tm_dots(col = "red", size = 0.05)

library(sp)
ptsreg <- spsample(g, 4000, type = "regular")   # Define the ouput grid - 4000 points in polygons extent
Krig = autoKrige(APPT~1,sp_mydata, new_data = ptsreg)$krige_output
Krig = Krig[!is.na(over(Krig,as(g,"SpatialPolygons"))),]  # take only the points falling in poolygons
Krig_df = as.data.frame(Krig)
names(Krig_df) = c("longitude","latitude", "APPT_pred","APPT_var","APPT_stdev")
g_fort = fortify(g)
Borders = ggplot() +
  geom_raster(data=Krig_df, aes(x=longitude, y=latitude,fill=APPT_pred))+
  geom_polygon(data=g_fort,aes(x=long,y=lat,group=group),
               fill='transparent',color = "black")+
  theme_bw()
Borders

map_500 <- ggplot()+
  geom_contour_filled(data = DataSynthesis, aes(x=Samples.Longitude,y=Samples.Latitude,z=OutBiomization.Biome), binwidth = 500)+ 
  geom_sf(data = world, fill = "transparent", color = "grey")+ 
  coord_sf(xlim=c(-60,200), ylim=c(10,90), expand = FALSE)+ 
  labs(title = "Isochrones Map of Europe and the Near East (TAPAS)", fill = "Isochrones") 

DataBiomization=data.frame(Samples$Longitude,Samples$Latitude,OutBiomization$Biome)
ggplot(DataBiomization)+
  geom_point(data=DataBiomization,aes(x=Samples.Longitude, y= Samples.Latitude, color = OutBiomization.Biome)) +
  scale_color_manual(values=c('#65B2FF','#BBCB35',"#E69F00","cyan", '#0020CA',"#55EB49","#CAFFCA","#000065","#FFB3A5","#E7E718","green","grey","yellow","blue",'red')) +
  labs(x="Longitude", y="Latitude", color = "Types", size="Types") +
  #geom_label_repel(data=map, aes(x=Longitude, y= Latitude,label=)) +
  theme_bw()


map=data.frame(Samples,OutBiomizationFin$Biome)
map=na.omit(map)
print(Output)
hist(Oustput$Biome)
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
head(worldmap[c('name', 'continent')])
ggplot() + geom_sf(data = worldmap) +
  coord_sf(xlim = c(-30, 150), ylim = c(20, 80), expand = FALSE) +
  geom_point(data=map,aes(x=Longitude, y= Latitude, color = Output.Biome)) +
  scale_color_manual(values=c('#65B2FF','#BBCB35',"#E69F00","cyan", '#0020CA',"#55EB49","#CAFFCA","#000065","#FFB3A5","#E7E718","green","grey","yellow","blue",'red')) +
  labs(x="Longitude", y="Latitude", color = "Biome", size="Types") +
  theme_bw()

#Hengl et al., 2018
library(raster)
library(mapplots)
Biome=raster("pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif")
Samples=read.csv2("LongLatWorldClim.csv",sep=";")
Samples=na.omit(Samples)
xy=data.frame(x=as.numeric(Samples$Longitude),y=as.numeric(Samples$Latitude),name=Samples$SiteName)
coordinates(xy)= ~ x + y
coord=data.frame(Samples$Longitude,Samples$Latitude)
ExtBiome=extract(Biome,coord)
Loc=data.frame(Samples$Longitude,Samples$Latitude,ExtBiome)
Loc=na.omit(Loc)
Data=data.frame(na.omit(Samples$Longitude),na.omit(Samples$Latitude),ExtBiome,Loc$DataSynthesis.BiomeNum)

Altitude=na.omit(read.csv2("Altitude_EMPD.csv"))
OutBiomizationComp=data.frame(Samples$Longitude,Samples$Latitude,OutBiomization$Biome$BiomeNum)
OutBiomizationComp=na.omit(OutBiomizationComp)

Comp=data.frame(Altitude,OutBiomizationComp,ExtBiome)
barplot(table(Comp$OutBiomization.Biome.BiomeNum[Comp$Elevation>2000]),col=c('#65B2FF','#BBCB35',"#E69F00","cyan", '#0020CA',"#55EB49","#CAFFCA","#000065","#E7E718","green","grey","blue",'#EE82EE','darkolivegreen'))
barplot(table(Comp$OutBiomization.Biome.BiomeNum[(Comp$Elevation<2000)&(Comp$Elevation>1000)]),col=c('#65B2FF','#BBCB35',"#E69F00","cyan", '#0020CA',"#55EB49","#CAFFCA","#000065","#E7E718","green","grey","blue",'#EE82EE','darkolivegreen'))
barplot(table(Comp$OutBiomization.Biome.BiomeNum[(Comp$Elevation<1000)&(Comp$Elevation>500)]),col=c('#65B2FF','#BBCB35',"#E69F00","cyan", '#0020CA',"#55EB49","#CAFFCA","#000065","#E7E718","green","grey","blue",'#EE82EE','darkolivegreen'))
barplot(table(Comp$OutBiomization.Biome.BiomeNum[(Comp$Elevation<1000)]),col=c('#65B2FF','#BBCB35',"#E69F00","cyan", '#0020CA',"#55EB49","#CAFFCA","#000065","#E7E718","green","grey","blue",'#EE82EE','darkolivegreen'))


#Diag pie
data(coast)
xyz=make.xyz(Comp$Samples.Longitude,Comp$Samples.Latitude,Comp$OutBiomization.Biome.BiomeNum,Comp$ExtBiome)
basemap(c(5,10),c(30,40))
draw.shape(coast,col="cornsilk")
draw.pie(xyz$x,xyz$y,xyz$z,radius=0.5)

###############################


xy=data.frame(x=as.numeric(Samples$Longitude),y=as.numeric(Samples$Latitude),name=Output$Biome)
coordinates(xy)=~x+y
hist(Output$Biome)
library(maptools)
library(mapdata)
library(maps)
map('worldHires', col='gray90', fill=T,xlim=c(-60,200), ylim=c(10,90))+
  points(Samples$Longitude, Samples$Latitude, pch=2)
col.regions=c('#1C5510',"#659208","#AE7D20","#000065",'#BBCB35', '#009A18',"#CAFFCA","#55EB49",'#65B2FF', '#0020CA' ,"#8EA228","#FF9AFD","#BAFF35","white","#FFBA95","#FFBA35","#F7FFCA","#E7E718","#798649","#65FF9A","#D29E96")
c('#65B2FF',"#55EB49","#65A","#CAFFCA","#55EB49")
library(ggmap)
MAP <- get_map(location='europe', zoom=4, maptype = "terrain", source='google', color='color')
ggmap(MAP) + geom_point(data = map , aes(x=Latitude, y=Longitude, color= Output.Biome))

library(sf)
library(here)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(magrittr)
world <- ne_countries(scale = "medium", returnclass = "sf")



ggplot(data=nato_countries) +
  ggplot2::geom_sf(data = nato_countries) 
  geom_point(data=map,aes(x=Longitude, y= Latitude, color = Output.Biome)) +
  scale_color_manual(values=c('#65B2FF','#BBCB35',"#E69F00","cyan", '#0020CA',"#55EB49","#CAFFCA","#000065","#FFB3A5","#E7E718","green","grey","yellow","blue",'red')) +
  labs(x="Longitude", y="Latitude", color = "Types", size="Types") +
  #geom_label_repel(data=map, aes(x=Longitude, y= Latitude,label=)) +
  theme_bw()
col.regions=c('#1C5510',"#659208","#AE7D20","#000065",'#BBCB35', '#009A18',"#CAFFCA","#55EB49",'#65B2FF', '#0020CA' ,"#8EA228","#FF9AFD","#BAFF35","white","#FFBA95","#FFBA35","#F7FFCA","#E7E718","#798649","#65FF9A","#D29E96")
  
for i (in 1:nrow(BPHT))
  {for (k in 1:ncol(BPHFT))
  {if ((BPHT)!=0)
   {}
sum(EMPD[,3][(EMPD[,1]==NameCal[i])&(EMPD[,2]==NamePol[j])])}
MaI=data.frame(matrix(NA,ncol=nrow(NamePFT),nrow=nrow(NamePolLGP)))
colnames(MaI)=NamePFT$PFTs
rownames(MaI)=NamePolLGP$Name
MaI$Name=c(rep(NameCal[1],times=nrow(MaI)))


for (i in 1:nrow(MaI))
{for (j in 1:colnames(MaI))
{Site=MaI$Name[1]
MaI[i,j]=which(colnames(Ass4)==Site)&(rownames(MaI)==colnames(Ass4))]}}


  
write.csv(ass3i,"AssM_LDB.txt")
write.csv(ass4,"AssM_LGP.txt")
write.csv(ass5,"AssM_Echets.txt")
write.csv(ass6,"AssM_Eifel.txt")
library(rgdal)
library(raster)
library(geodata)
devtools::install_github('oscarperpinan/rasterVis') 
library(rasterVis)
library(terra)
library(maptools)
library(maps)
library(sp)
library("lattice")
library("latticeExtra")
library("unmarked")
library(terra)
#library(gplot)

worldclim_tile(var='bio',res=0.5,lon=45, lat=10, path=tempdir(), version="2.1")
Samples=read.csv2("LongLatWorldClim.csv",sep=";")
#pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1
Biome=raster("pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif")

ATR=raster("wc2.1_30s_bio_7.tif")
MTWM=raster("wc2.1_30s_bio_5.tif")
MTCM=raster("wc2.1_30s_bio_6.tif")
TANN=raster("wc2.1_30s_bio_1.tif")
GA=raster("ai_v3_yr.tif")
Samples=na.omit(Samples)
#x=c('#1C5510','#65B2FF', '#009A18', '#0020CA',"#CAFFCA",'#BBCB35',"#F7FFCA","#798649","#E7E718","#65FF9A","#D29E96","#FFBA35","#65EB49","#FF9ADF","#8EA228","#AE7D20","#1C5510","#BAFF35","#659208","#000065","#FFBA9A")

xy=data.frame(x=as.numeric(Samples$Longitude),y=as.numeric(Samples$Latitude),name=Samples$SiteName)
coordinates(xy)= ~ x + y

myCol=terrain.colors(32)
levelplot(Biome,col.regions=c('#1C5510',"#659208","#AE7D20","#000065",'#BBCB35', '#009A18',"#CAFFCA","#55EB49",'#65B2FF', '#0020CA' ,"#8EA228","#FF9AFD","#BAFF35","white","#FFBA95","#FFBA35","#F7FFCA","#E7E718","#798649","#65FF9A","#D29E96"))
hist(Biome)
BiomeExt=extract(Biome,xy)
hist(BiomeExt,col=c("#000065",'#009A18',"#CAFFCA","#55EB49", '#0020CA' ,"#8EA228","#FF9AFD","#FFBA9A","#FFBA35","#F7FFCA","#E7E718","#798649","#65FFA9"))
biome_Plus=data.frame(Samples$Longitude,Samples$Latitude,BiomeExt)

library(maptools)
library(mapdata)
library(maps)


library(sp)
library(sf)
library(raster)
theme_set(theme_bw())
ggplot() +
  geom_sf(data = biome_Plus, aes(color = BiomeExt), size = 1)

r <- raster(nrow=nrow(biome_Plus), ncol=5, xmn=0, xmx=10, ymn=0, ymx=10, crs="")
set.seed(1)
values(r) <- as.matrix(Samples$Longitude,Samples$Latitude)
xyz <- rasterToPoints(r)
coordinates(points)= ~ as.numeric(biome_Plus$Samples.Longitude) + as.numeric(biome_Plus$Samples.Latitude)

proj4string(pts)=CRS("+init=epsg:4326") # set it to lat-long
pts = spTransform(pts,CRS("insert your proj4 string here"))

map('worldHires', col='gray90', fill=T,xlim=c(-60,200), ylim=c(10,90))+
  points(biome_Plus$Samples.Longitude, biome_Plus$Samples.Latitude, pch=16,col='blue')

hist(biome_Plus$BiomeExt)

BLGS=raster("pnv_bLGS.tif")
CDF=raster("pnv_CDF.tif")
CENF=raster("pnv_CENF.tif")
CENFb=raster("pnv_CENFb.tif")
CF=raster("pnv_CF.tif")
CMF=raster("pnv_CMF.tif")
CTR=raster("pnv_CTR.tif")
Desert=raster("pnv_Desert.tif")
EDST=raster("pnv_EDST.tif")
GFT=raster("pnv_GFT.tif")
PDST=raster("pnv_PDST.tif")
SSE=raster("pnv_SSE.tif")
Steppe=raster("pnv_Steppe1.tif")
TDBFW=raster("pnv_TDBFW.tif")
TDBF=raster("pnv_TDBF.tif")
TEBF=raster("pnv_TEBF.tif")
TENOW=raster("pnv_TENOW.tif")
TS=raster("pnv_TS.tif")
TSEBF=raster("pnv_TSEBF.tif")
TSWS=raster("pnv_TSWS.tif")
WTENF=raster("pnv_WTENF.tif")
XW=raster("pnv_XW.tif")
r=merge(CDF,CENF)

hist(Biome,breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32))
levelplot(Biome,maxpixels=1e7,at=c(1,2,3,4,7,8,9,13,14,15,16,17,18,19,20,22,27,28,30,31,32),col.regions=c('#1C5510',"#659208","#AE7D20","#000065",'#BBCB35', '#009A18',"#CAFFCA","#55EB49",'#65B2FF', '#0020CA' ,"#8EA228","#FF9AFD","#BAFF35","white","#FFBA95","#FFBA35","#F7FFCA","#E7E718","#798649","#65FF9A","#D29E96"))
levelplot(Biome,maxpixels=1e7,at=c(1,2,3,4,7,8,9,13,14,15,16,17,18,19,20,22,27,28,30,31,32),col.regions=c('#1C5510',"#659208","#AE7D20","#000065",'#BBCB35', '#009A18',"#CAFFCA","#55EB49",'#65B2FF', '#0020CA' ,"#8EA228","#FF9AFD","#BAFF35","white","#FFBA95","#FFBA35","#F7FFCA","#E7E718","#798649","#65FF9A","#D29E96"),xlim=c(-20,120),ylim=c(30,80),scales=list(200,200))+
  latticeExtra::layer(sp.points(xy, cex = 0.5,col=1,alpha=0.3))
ExtBiome=raster::extract(Biome,coord)
ExtBiome=na.omit(ExtBiome)
hist(ExtBiome,col=c("#000065",'#009A18',"#CAFFCA","#55EB49",'#65B2FF', '#0020CA' ,"#8EA228","#FF9AFD","#FFBA95","#FFBA35","#F7FFCA","#E7E718","#798649","#65FF96"))



levelplot(MTCM,par.settings =BuRdTheme,maxpixels=1e6,scales=list(200,200),xlim=c(-20,120),ylim=c(30,80),at=seq(-80,60,length=100))+
  latticeExtra::layer(sp.points(xy, cex = 0.5,col=1,alpha=0.3))
levelplot(MTWM,par.settings =BuRdTheme,maxpixels=1e6,scales=list(200,200),xlim=c(-20,120),ylim=c(30,80),at=seq(-80,60,length=100))+
  layer(sp.points(xy, cex = 0.5,col=1,alpha=0.3))
levelplot(TANN,par.settings =BuRdTheme,maxpixels=1e6,scales=list(200,200),xlim=c(-20,120),ylim=c(30,80),at=seq(-80,60,length=100))+
  layer(sp.points(xy, cex = 0.5,col=1,alpha=0.3))
levelplot(ATR,par.settings =PuOrTheme,maxpixels=1e6,scales=list(200,200),xlim=c(-20,120),ylim=c(30,80),at=seq(0,80,length=100))+
  latticeExtra::layer(sp.points(xy, cex = 0.5,col=1,alpha=0.3))
library(RColorBrewer)
myTheme=rasterTheme(region=brewer.pal(4,"BuGn"))
levelplot(GA,par.settings =myTheme,maxpixels=1e6,scales=list(200,200),xlim=c(-20,120),ylim=c(30,80),at=seq(0,70000,length=100))+
  layer(sp.points(xy, cex = 0.5,col=1,alpha=0.3))



All=data.frame(MTCM,MTWM,TANN)
na.omit()
head(MTCM)
setnames(
  paste0("tmax_Jan_09.", 1:5),
  seq(ymd("2009-01-01"), ymd("2009-01-05"), by = "days") %>%
    as.character()
)

library(terra)
library(gplot)
layout(matrix(1:4, ncol = 2))
ggplot() +
  geom_raster(data = , aes(x = x, y = y, fill = tmax)) +
  facet_wrap(date ~ .) +
  coord_equal() +
  scale_fill_viridis_c() +
  theme_void() +
  theme(
    legend.position = "bottom"
  )

plot(MTCM,xlim=c(-100,200),ylim=c(10,85),npretty=7)+
  points(Samples$Longitude,Samples$Latitude,pch=2,cex=0.1,col='blue')


plot(MTWM,xlim=c(-15,70),ylim=c(30,70))
plot(TANN,xlim=c(-15,70),ylim=c(30,70))
plot(GA,xlim=c(-15,70),ylim=c(30,70))
layout(1)
Samples=read.csv2("LongLatWorldClim.csv",sep=";")
coord=data.frame(Samples$Longitude,Samples$Latitude)
ExtMTCM=extract(MTCM,coord)
ExtMTWM=extract(MTWM,coord)
ExtTANN=extract(TANN,coord)
ExtATR=extract(ATR,coord)
ExtGA=extract(GA,coord)
bioPlus=data.frame(Samples$Longitude,Samples$Latitude,ExtTANN,ExtMTCM,ExtMTWM,ExtATR,ExtGA)

library(ggplot2)
ggplot((bioPlus),aes(ExtATR))+
  geom_density(alpha=0.3)+
  theme_classic()
write.csv2(bioPlus,"bio_plus.csv")

library(sf)
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(readxl)
library(bookdown)
library(rmdformats)
plot(GA) + 
  ggtitle("Répartition des centres d'état civil sécondaires par département") +
  theme_void() 
library(tidyverse)
library(ggmap)
dtst <- bioPlus %>%
  separate(Coordonnées, into = c("Latitude","Longitude"), sep = ".") %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) 
