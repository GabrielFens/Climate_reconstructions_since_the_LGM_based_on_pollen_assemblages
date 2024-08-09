Age_Furamoos=read.csv2("ModeleAge_Furamoos.csv",sep=',',dec='.')
PCA_Furamoos=read.csv2("PCA_Furamoos.csv",sep=',',dec='.')
Age_Eifel=read.csv2("ModeleAge_Eifel.csv",sep=';',dec=',')
PCA_Eifel=read.csv2("PCA_Eifel.csv",sep=',',dec='.')
Age_LDB=read.csv2("ModeleAge_LDB.csv",sep=';',dec=',')
PCA_LDB=read.csv2("PCA_LDB.csv",sep=',',dec='.')
Age_LGP=read.csv2("ModeleAge_LGP.txt",sep=',',dec='.')
PCA_LGP=read.csv2("PCA_LGP.csv",sep=',',dec='.')
Age_Echets=read.csv2("ModeleAge_Echets.csv",sep=',',dec='.')
PCA_Echets=read.csv2("PCA_Echets.csv",sep=',',dec='.')
Ens=data.frame(matrix(NA,ncol=10,nrow=3001))
Ens[1:nrow(Age_Furamoos),1]=Age_Furamoos$X2/1E3
Ens[1:nrow(PCA_Furamoos),2]=PCA_Furamoos$x
Ens[1:nrow(Age_Eifel),3]=Age_Eifel$Age
Ens[1:nrow(PCA_Eifel),4]=PCA_Eifel$x
Ens[1:nrow(Age_LGP),5]=Age_LGP$X2/1E3
Ens[1:nrow(PCA_LGP),6]=PCA_LGP$x
Ens[1:nrow(Age_Echets),7]=Age_Echets$X2/1E3
Ens[1:nrow(PCA_Echets),8]=PCA_Echets$x
Ens[1:nrow(Age_LDB),9]=Age_LDB$UncerAge_Bacon/1E3
Ens[1:nrow(PCA_LDB),10]=PCA_LDB$x

ggplot(Ens,aes(X5,X6))+
  #geom_point(shape=8, color='black', size=1)+
  geom_line(aes(X9,X10),color='red')+
  #geom_line(aes(X3,X4),color='red')+
  #geom_line(aes(X5,X6),color='yellow')+
  #geom_line(aes(X7,X8),color='green')+
  #geom_line(aes(X9,X10))+
  theme_classic()
AgeEns=data.frame(Ens$X1,Ens$X3)

plot(Age_LDB$UncerAge_Bacon,PCA_LDB$x)
plot(Age_Eifel$Age,PCA_Eifel$x)
