### Name: recMAT
### Title: Transfer function by modern analogues method
#ass = modern assemblage matrix - 17 taxons observés et à comparer avec le spectre fossile 28LGP
#clim = modern climate - T-ann & P-ann
#fass = fossil assemblages - EMPD2 2020 & 2021
#ichd = option for the distance (0=euclidian /1=chord /2=log /3=canberra) - Choix du reste de la division euclidienne
#kana = maximum number of analogues - Fixer à 7
#wghts = coefficient vector to weight the taxa in the computation of the distance
#mcon = category of the modern data (biome to constrain the analogue selection)
#fcon = category of the fossil data
#fass<-read.csv2("popil_DataMAT.txt",sep='',dec=',')
fass<-read.csv2("fospop.txt",sep="",dec='.')
fass2<-read.csv2("popil_DataMAT.csv",sep=';',dec=',')
ass<-read.csv2("ss700po.txt",sep='',dec='.')
ass2<-read.csv2("ModernData.csv",sep=';',dec=',')
SiteName<-read.csv2("SiteName_ModernData.csv",sep=";",dec='.')
row.names(ass2)=SiteName$Name
#ass<-read.csv2("ss700po.txt",sep='\t',dec='.')
clim<-read.csv2("ss700cl.txt",sep='',dec='.')
clim2<-read.csv2("ModernClimate.csv",sep=';',dec=',',na.strings='NA')
row.names(clim2)=SiteName$Name
#Checker les dimenssions des listes pour analogie
print(ncol(ass2))
print(ncol(fass2))
print(nrow(ass2))
print(nrow(clim2))
print(fass)


#------Minimisation des distances analogues
"bestanal" <-
  function (ref0,fos0,option=0,wghts=NULL,mcon=NULL,fcon=NULL) {
    ref=ref0
    fos=fos0
    nref<-nrow(ref)
    nfos<-nrow(fos)
    m<-ncol(fos)
    # weights
    if (is.null(wghts)) {wghts=rep(1/m,m)}
    wghts=wghts/sum(wghts)
    W=diag(wghts)
    
    # constraints
    if (is.null(mcon) | is.null(fcon)) {
      mcon=rep(" ",nrow(ref0))
      fcom=rep(" ",nrow(fos0))}
    
    # init
    if (identical(m,ncol(ref))==FALSE) stop ("both matrices ref and fos have not same number of columns")
    mdist="euclidian"
    if(option==3) {mdist="canberra"}
    matrice.dist<-rep(0,nref)
    matrice.sort<-rep(0,nref)
    anal<-list(title="list of best analogues followed by their distances")
    anal$list<-matrix(data=0,ncol=nref,nrow=nfos)
    anal$dist<-matrix(data=0,ncol=nref,nrow=nfos)
    anal$seuil<-NA
    term=5
    if(max(ref)<=1) {term=0.005}
    if(max(ref)>1 & max(ref)<=100) {term=0.5}
    term1=term/5
    # transformation
    if (option==1) {
      ref <-sqrt(ref )
      fos<-sqrt(fos) }
    if (option==2) {
      #		ref <-log((ref-min(ref))/(max(ref)-min(ref))*1000+1 ) 
      #		fos <-log((fos-min(fos))/(max(fos)-min(fos))*1000+1 )  }
      #		ref[ref<term1]=term1
      #		fos[fos<term1]=term1
      ref <-log(ref+term1)
      fos<-log(fos+term1) }
    for (i in 1:nfos){
      fos2<-matrix(as.matrix(fos[i,]),nref,m,byrow=TRUE)
      if (option<=2) {
        dd<-(as.matrix(ref)-fos2)^2 
      } else {
        dd<-abs(as.matrix(ref)-fos2 )/pmax(ref+fos2,matrix(0.00001,nref,m))
      }
      dd=dd%*%W
      matrice.dist <- apply(dd,1,sum,na.rm=TRUE)
      matrice.dist[which(mcon!=fcon[i])]=1e+10
      matrice.sort <- order(matrice.dist,na.last=TRUE)
      if (length(matrice.sort)>0) {
        anal$list[i,1:length(matrice.sort)]<-matrice.sort
        matrice.sort <- sort(matrice.dist,na.last=TRUE)
        anal$dist[i,1:length(matrice.sort)] <- matrice.sort
      }
    }
    # seuil
    dalea<-rep(0,nref)
    if (option<=2) {
      for (i in 1:nref) {
        i2<-sample(1:nref,2)
        ii<-i2[1]
        if (i2[1]==i) {ii<-i2[2]}
        dal<-(ref[i,]-ref[ii,])^2
        dalea[i]=sum(as.matrix(dal)%*%W) }
    } else {
      for (i in 1:nref) {
        i2<-sample(1:nref,2)
        ii<-i2[1]
        if (i2[1]==i) {ii<-i2[2]}
        dal<-abs(ref[i,]-ref[ii,])/pmax(ref[i,]+ref[ii,],0.00001) }
      dalea[i]=sum(as.matrix(dal)%*%W)
    } 
    anal$seuil<-as.numeric(quantile(sample(dalea,1000,replace=TRUE),0.25,na.rm=TRUE))
    cat("Threshold adopted (1st quartile of random distances) =",anal$seuil,"\n")
    return(anal)
  }
#---- climate reconstruction from best analogues by weighted mean
# climate reconstruction from best analogues by weighted mean

WA_reconstruction <- function(y,analdist,analist,kana,seuil) 
{
  nz<-nrow(analdist)
  my<-ncol(y)
  ny<-nrow(y)
  ye<-list(title="matrix of y reconstruction: rmean=weighted mean of the analogues / rmin, rmax = resp. min and max of the analogues retained")
  ye$rmean <- matrix(NA,ncol=my,nrow=nz)
  ye$rmin <- matrix(NA,ncol=my,nrow=nz)
  ye$rmax <- matrix(NA,ncol=my,nrow=nz)
  ye$nbanal <- rep(NA,nz)
  ye$mindist <- rep(NA,nz)
  ye$maxdist <- rep(NA,nz)
  ye$analist=matrix(NA,nz,kana)
  ye$analdist=matrix(NA,nz,kana)
  
  for (i in 1:nz) {
    if (analdist[i,1]==0) {
      analdist[i,1:kana]=analdist[i,2:(kana+1)]
      analist[i,1:kana]=analist[i,2:(kana+1)]
    }
    poids <- 1/analdist[i,]
    k2<-0
    for (k in 1:kana) {
      if (analdist[i,k]<=seuil & analist[i,k]>0) {k2=k}
    }
    if (k2>=1) {
      for (j in 1:my) {
        ye$rmean[i,j]=poids[1:k2] %*% y[ analist[i,1:k2], j]
        somme=sum(poids[1:k2])
        ye$rmean[i,j]<-ye$rmean[i,j]/somme
        ye$rmin[i,j]<-min(y[analist[i,1:k2],j])
        ye$rmax[i,j]<-max(y[analist[i,1:k2],j])
      }
      ye$nbanal[i]<-k2
      ye$mindist[i]<-analdist[i,1]
      ye$maxdist[i]<-analdist[i,k2]
    }
    ye$analist[i,]=analist[i,1:kana]
    ye$analdist[i,]=analdist[i,1:kana]
  }
  return(ye)
}


str(clim)
str(ass)
str(fass2)
"recMAT" <- function(ass,clim,fass,fclim=FALSE,ichd=0,kana=7,wghts=NULL,mcon=NULL,fcon=NULL,outfile=NULL)
{
  # Initialisation
  
  pasdeseuil=FALSE
  if (kana<0) 
  {
    kana=abs(kana)
    pasdeseuil=TRUE
  }
  na<-nrow(ass)
  ma<-ncol(ass)
  nc<-nrow(clim)
  mc<-ncol(clim)
  nfa<-nrow(fass)
  mfa<-ncol(fass)
  if (identical(na,nc)==FALSE) { stop ("both ass & clim matrices have not same number of rows")}
  if (identical(mfa,ma)==FALSE) { stop ("both ass & fass matrices have not same number of columns")}
  MAT<-list(title="Transfer function estimates by MAT method")
  MAT$rmse<-rep(NA,mc)
  MAT$r2<-rep(NA,mc)
  MAT$rmsep<-rep(NA,mc)
  MAT$r2p<-rep(NA,mc)
  MAT$CE<-rep(NA,mc)
  MAT$clim<-matrix(NA,nc,mc*3)
  MAT$fclim<-matrix(NA,nfa,mc*3)
  MAT$nbanal<-matrix(0,nfa)
  MAT$mindist<-matrix(NA,nfa)
  MAT$maxdist<-matrix(NA,nfa)
  MAT$analist<-matrix(NA,nfa,kana)
  MAT$anadist<-matrix(NA,nfa,kana)
  MAT$ananames<-matrix(NA,nfa,kana)
  
  option<-"extrapolation"
  
  if (fclim != FALSE) 
  {
    nfc<-nrow(fclim)
    mfc<-ncol(fclim)
    print(c(nfc,nfa))
    if (identical(nfc,nfa)==FALSE) {stop ("both fclim & fass matrices have not same number of rows")}
    option<-"verification"
  }
  if (!is.null(outfile)) 
  {
    fgraph=paste(outfile,".pdf",sep="")
    ftext=paste(outfile,".txt",sep="")
    sink(file=ftext)
    pdf(file=fgraph)
  }
  if (is.null(mcon)) {mcon=rep(NA,na)}
  if (is.null(fcon)) {fcon=rep(NA,nfa)}
  MAT$fcon=fcon
  cat("TRANSFER FUNCTION : Modern analogues technique (MAT) \n")
  cat("----------------------------------------------------------------------------------------\n")
  mdist<-"euclidian"
  if(ichd==3) { mdist="canberra" }
  #Display input parameters in a file
  cat("max number of analogues = ",kana,"\n")
  cat("distance option (0=euclidian /1=chord /2=log /3=canberra) = ",ichd,"\n")
  cat("Weights used for each taxon = \n",wghts,"\n")
  # data characteristics
  nfc<-0
  mfc<-0
  option="extrapolation"
  if (fclim!=FALSE)
  {
    nfc<-nrow(fclim)
    mfc<-ncol(fclim)
    if (identical(nfc,nfa)==FALSE) {stop ("both fclim & fass matrices have not same number of rows")}
    option="verification"
  }
  # beginning of analogue search for the calibration data
  bac <- bestanal (ass,ass,ichd,wghts=wghts,mcon=mcon,fcon=mcon)
  seuil=bac$seuil
  if (pasdeseuil==TRUE) {seuil=seuil*100} 
  ye <- WA_reconstruction (clim,bac$dist,bac$list,kana,seuil)
  bav <- bestanal (ass,fass,ichd,wghts=wghts,mcon=mcon,fcon=fcon) 
  yp <- WA_reconstruction (clim,bav$dist,bav$list,kana,seuil)
  for (i in 1:nfa) 
  {
    for (k in 1:kana) { MAT$ananames[i,k]<-rownames(ass)[yp$analist[i,k]] }
  }
  MAT$analist=yp$analist
  MAT$anadist=yp$analdist
  cat("In the verification step, r2p (or reduction of error) compares the predictions to the calibration period climatology\n") 
  cat("In the verification step, CE (or coefficient of efficiency) compares the predictions to the verification period climatology\n") 
  #loop on the climatic variables
  for (j in 1:mc) 
  {
    cat("\nDependent Variable : ",colnames(clim)[j],"\n")
    cat("----------------------------------------\n")
    jj<-(j-1)*3
    MAT$clim[,jj+1]<-ye$rmean[,j]
    MAT$clim[,jj+2]<-ye$rmean[,j]-ye$rmin[,j]
    MAT$clim[,jj+3]<-ye$rmax[,j]-ye$rmean[,j]
    MAT$fclim[,jj+1]<-yp$rmean[,j]
    MAT$fclim[,jj+2]<-yp$rmean[,j]-yp$rmin[,j]
    MAT$fclim[,jj+3]<-yp$rmax[,j]-yp$rmean[,j]
    if (j==1) 
    {
      MAT$nbanal<-yp$nbanal
      MAT$mindist<-yp$mindist
      MAT$maxdist<-yp$maxdist
    }
    MAT$rmse[j]<-mean((clim[,j]-ye$rmean[,j])^2,na.rm=TRUE)^0.5
    MAT$r2[j]<-1-(MAT$rmse[j]/sd(clim[,j],na.rm=TRUE))^2
    cat("R2 = ",MAT$r2[j],"  -- RMSE = ",MAT$rmse[j],"calculated on ",na,"  observations\n")
    smin<-min(clim[,j],na.rm = T)
    smax<-max(clim[,j],na.rm = T)
    if (option!="verification") {
      plot(c(smin,smax),c(smin,smax), type='l',xlab=paste(colnames(clim)[j]," calib"), ylab="Predicted MAT", main="",col="green")
      points(clim[,j],MAT$clim[,(jj+1)],pch=19,col='blue')
    }	
    # verification
    if (option=="verification") 
    {
      MAT$rmsep[j]<-mean((fclim[,j]-yp$rmean[,j])^2,na.rm=TRUE)^0.5
      MAT$r2p[j]=1-MAT$rmsep[j]^2/mean((fclim[,j]-mean(clim[,j],na.rm=TRUE))^2)
      MAT$CE[j]= 1-(MAT$rmsep[j]/sd(fclim[,j],na.rm=TRUE))^2
      cat("R2p (RE) = ",MAT$r2p[j],"  -- CE = ",MAT$CE[j],"  -- RMSEp = ",MAT$rmsep[j],"calculated on ",nfa,"  observations\n")
      smin<-min(cbind(t(clim[,j]),t(fclim[,j])),na.rm = T)
      smax<-max(cbind(t(clim[,j]),t(fclim[,j])),na.rm = T)
      plot(c(smin,smax),c(smin,smax), type='l',xlab=paste(colnames(clim)[j]," calib (blue) / verif (red)"), ylab="Predicted MAT", main="",col="green")
      points(clim[,j],MAT$clim[,(j-1)*3+1],pch=19,col='blue')
      points(fclim[,j],MAT$fclim[,(j-1)*3+1],col='red',pch=21)
    }
    else
    {
      # extrapolation
      cat("the estimated climate is put in the MAT$fclim matrix(reconstruction, lower bar, upper bar)\n")
    }
  }
  coln<-rep(NA,mc*3)
  for (j in 1:mc) {
    coln[(j-1)*3+1]<-colnames(clim)[j]
    coln[(j-1)*3+2]<-paste(colnames(clim)[j],'_i',sep="")
    coln[(j-1)*3+3]<-paste(colnames(clim)[j],'_s',sep="")
  }
  colnames(MAT$fclim)<-coln
  colnames(MAT$clim)<-coln
  rownames(MAT$fclim)<-rownames(fass)	
  rownames(MAT$clim)<-rownames(clim)	
  rownames(MAT$analist)<-rownames(fass)
  rownames(MAT$anadist)<-rownames(fass)
  rownames(MAT$ananames)<-rownames(fass)
  names(MAT$rmse)<-colnames(clim)
  names(MAT$r2)<-colnames(clim)
  names(MAT$rmsep)<-colnames(clim)
  names(MAT$r2p)<-colnames(clim)
  names(MAT$CE)<-colnames(clim)
  cat("\nList MAT\n")
  cat("MAT$analist contains the list of the analogues for each extrapolation sample\n")
  cat("MAT$anadist contains the list of the analogue distance for each extrapolation sample\n")
  print(MAT)
  if (!is.null(outfile)) 
  {
    dev.off()
    sink()
    sink()
    sink()
    print("end")
  }
  return(MAT)
}

str(fass)
recon<-recMAT(ass2,clim2,fass2,fclim=FALSE,ichd=0,kana=9,wghts=NULL,mcon=NULL,fcon=NULL,outfile=NULL)
Anal<-bestanal (ass,ass,0,wghts=NULL,mcon=NULL,fcon=NULL)

library(ggplot2)
str(recon$fclim)
write.csv(ass,"AssTest.txt")
write.csv(recon$fclim,"fclim.txt")
climateRecon<-read.csv2("fclim.txt",sep=',',dec='.')
crest_ex<-read.csv2("popil_Data.csv",sep=';',dec=',')
Index<-seq(0,nrow(climateRecon)-1,1)
print(length(Index))
print(Index)
print(climateRecon$T_ann)

Age<-crest_ex$Age
str(Age)
Age<-Age/1000
str(climateRecon$T_ann)
print(class(Age))
str(climateRecon$T_ann)
T_ann=climateRecon$T_ann
T_ann_i=climateRecon$T_ann_i
T_ann_s=climateRecon$T_ann_s
climateRecon1<-data.frame(Age,T_ann,T_ann_i,T_ann_s)
ggplot(na.omit(climateRecon1),aes(Age,T_ann))+
  ggtitle(label="Climate Reconstruction based on Pollen record of La Grande Pile",subtitle=" Temperature via MAT/ichd-EMPD2/Oxcal")+
  xlab("Age (Kyr BP)") +
  ylab("Mean Annual Temperature (°C)") +
  geom_point(shape=4, color='black', size=3)+
  geom_line(color="blue", lwd = 3,alpha=0.5)+
  geom_pointrange(aes(ymin=T_ann-T_ann_i,ymax=T_ann+T_ann_s),alpha=0.5)+
  ylim(-5,20)+
  theme_classic()
P_ann=climateRecon$P_ann
P_ann_i=climateRecon$P_ann_i
P_ann_s=climateRecon$P_ann_s
climateRecon2<-data.frame(Age,P_ann,P_ann_i,P_ann_s)
ggplot(na.omit(climateRecon2),aes(Age,P_ann))+
  geom_line(color="blue", lwd = 3,alpha=0.5)+
  ggtitle(label="Climate Reconstruction based on Pollen record of La Grande Pile",subtitle="Precipitation via MAT/ichd-EMPD2/Oxcal")+
  xlab("Age (Myr)") +
  ylab("Mean Annual Precipitation (mm)") +
  geom_point(shape=4, color='black', size=3)+
  geom_pointrange(aes(ymin=P_ann-P_ann_i,ymax=P_ann+P_ann_s),alpha=0.5)+
  theme_classic()

