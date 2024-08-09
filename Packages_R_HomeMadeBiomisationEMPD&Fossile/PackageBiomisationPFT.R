####################### GF Biomisation modeling - 2024
library(ggplot2)
library(plotly)
library(dplyr)
library(sf)
library(rnaturalearth)
library(stringr)

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

#Begin-Fossil
#Dataset-All were already normalized
Name=read.csv2("NameLGP.txt",sep='\t',dec=',')
AssPol=read.csv2("AssFLGP.csv",sep=';',dec=',') 
AssPol=AssPol[,-c(1)]
AssPol=AssPol*100
colnames(AssPol)=Name$Names
AgesCal=read.csv2("ModeleAge_LGP.txt",sep=',',dec='.')
Ages=AgesCal$X2
namefile="OutLGP_Biomisation_1"

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








