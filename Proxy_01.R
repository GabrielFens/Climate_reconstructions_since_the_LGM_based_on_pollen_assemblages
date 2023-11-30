##crest (v.1.3.0)
#Installations des packages et librairies utilisées 
library(devtools)
library(DBI)
library(RSQLite)
library(odbc)
library(RMySQL)
db<-datasetsDb()
devtools::install_github("r-dbi/odbc")
if(!require(devtools)) install.packages("devtools")

#devtools::install_version("crestr", version = "1.2.0", repos = "https://cran.us.r-project.org")
devtools::install_github("mchevalier2/crestr",force=TRUE)
#dbDownload(dbname='gbif4crest_02-5min.sqlite3',out='path/to/db')
#db <- DBI::dbConnect(RSQLite::SQLite(), dbname = "gbif4crest_02-5min.sqlite3")
#con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQLite3 ODBC Driver};")
con <- dbConnect(odbc::odbc(),"gbif4crest_02-5min.sqlite3")
con <- dbConnect(drv=RSQLite::SQLite(), dbname="gbif4crest_02-5min.sqlite3")
str(con)
print(dbListTables(con))
print(dbListFields(con,"taxa"))
print(dbGetQuery(db,"select * from taxa"))

#download.file(paste('gbif4crest_02-5min.sqlite3'))
library(crestr)
getTaxonomy(family='Lamiaceae',depth.out=6)
str(crest_ex)
dbSubset(taxaType=1)
accCountryNames('Europe')
accClimateVariables()  
#Afficher les données
data(crest_ex)
data(crest_ex_pse)
str(crest_ex_pse)
data(crest_ex_selection)
str(crest_ex_selection)
print(crest_ex_selection)
createPSE(colnames(crest_ex))
db <- RSQLite::datasetsDb()
crest.get_modern_data()
#Différentes taxonomies avec leur âge (courbe de calibration)
head(crest_ex)
#Structure de la dataframe 
str(crest_ex)
#Variables climatiques traitées pour chaque taxonomie 
crest_ex_pse
#Jeu de données pour chaque variable reconstituée 
crest_ex_selection
str(crest_ex_selection)
getTaxonomy(species='Aconogonon Alpinum x Weyrichii',dbname="gbif4crest_02-5min.sqlite3")
print(crest_ex_selection)

#Fonction Crest pour produire des pseudo-données et fournie certains paramètres 
crest_ex<-read.csv2("popil_Data.csv",sep=';',dec=',')
crest_pse<-read.csv2("proxy_species_equivalency.csv",sep=';')
crest_exb<-read.csv2("LGP_Beaulieu2.csv",sep=';',dec=',')
crest_pseb<-read.csv2("proxy_species_equivalency2.csv",sep=';')

#dbSubset(dbname='gbif4crest_02',taxaType=1,out='path/to/dbsubset')
print(crest_ex)
print(crest_pse)
rcnstrct<-crest.get_modern_data(df=crest_ex,climateWithObs=FALSE,pse=crest_pse,taxaType=1,climate=c('bio1','bio12'),xmn=10,xmx=140,ymn=50,ymx=90,continents='Europe',minGridCells=20,site_info=c(47.81,6.51),dbname='gbif4crest_02-5min.sqlite3',verbose='TRUE')
recon<-crest.calibrate(rcnstrct,shape=c('normal','lognormal'),bin_width =c(2,150),npoints=500,verbose=TRUE)
plot(recon)
plot_climateSpace(recon)
plot_scatterPDFs(recon,xlim=c(-25,30),ylim=c(0,2000),filename='FigureScatter',uncertainties = 0.5)
plot_violinPDFs(recon,filename='FigureViolin.png')
plot_taxaCharacteristics(recon,taxanames='Abies')
plot_low(recon)
plot_diagram(recon)
recon<-crest.reconstruct(recon,taxWeight='normalisation',presenceThreshold=0,verbose=TRUE)
plot(recon, climate = 'bio1')
plot(recon, climate = 'bio12')
plot(recon,simplify =FALSE)
plot_combinedPDFs(recon,samples=c(1),climate='bio1',only.present=TRUE,only.selected=FALSE)
plot_low(recon)
#Exportation des données
export(recon, dataname='final_reconGP_11Cas',fullUncertainties = FALSE,pdfs=FALSE)

#plot(recons)
#names(recon)
recons <- crest(df = crest_ex, pse = crest_pse, taxaType = 0,
  site_info = c(7.5, 7.5), site_name = 'crest_example',
  climate = c("bio1", "bio12"), bin_width = c(2, 50),
  shape = c("normal", "lognormal"),
  dbname = "crest_example")
recon<-crest
#Afficher les différentes variables Data : tax, géographie et climatique
str(recons)
#Représentation des pseudo-données 
plot_climateSpace(recons)
#Data géographique (Taxa)
#Tax 2 versus bio1 a une corrélation plus forte que Tax2 versus bio12
plot_taxaCharacteristics(recons, taxanames='Taxon2')
plot_taxaCharacteristics(recons, taxanames='Taxon3')
plot_taxaCharacteristics(recons, taxanames='Taxon4')
plot_violinPDFs(recons,filename='FigureViolin.png')
#Afficher les variables plotées
names(recons)
#Caractéristiques des variables climatiques (bio)
lapply(recons$reconstructions, names)
#Afficher les noms des colonnes
head(recons$reconstructions$bio1$optima)
str(recons$reconstructions$bio1$optima)
signif(recons$reconstructions$bio1$likelihood[1:6, 1:6], 3)
#Représentation des données (Temp versus Temps)
plot(recons, climate = 'bio1')
plot(recons, climate = 'bio12', simplify=TRUE, uncertainties=c(0.4, 0.6, 0.8))
#Exporter les data
export(recons, loc=getwd(), dataname='crest-test')
list.files(file.path(getwd(), 'crest-test'))
#Extraire les données de calibration
recon<- crest.calibrate(recons, shape=c('normal','lognormal'),bin_width=c(2,10))
plot(recon)
#Etablir la relation proxy-climat
recon<-crest.reconstruct(recon,taxWeight='normalisation',presenceThreshold=0)
plot(recon)
recon<-loo(recon)
str(recon)
plot(recon)
crest.get_modern_data()
#includeTaxa(recon,taxa=c('taxa1','taxa2'),climate='bio1')
#getspeciesDiversity(recon)

Data_frame<-read.delim("popil_Data.txt",sep='\t',dec='.',header=TRUE)
#entete<-read.delim("entete_variable.txt", sep = "", header = FALSE, fileEncoding = "UTF-8", comment.char = "/")
#colnames(donnees) <- make.names(tolower(entete[1:5, "V1"]), unique = TRUE)
str(Data_frame)
#Datanum<-as.numeric(Data)
#str(entete)
#names(Data)
#colnames(Data)[0] <- c("Prof")
str(colnames(Data_frame)[0])
#str(setNames(Data_frame,c("changed_Col1","changed_Col2","changed_Col3")))
#Tableau <- read.table(Data, header = TRUE)[0:8]
#colnames(Data) <- c("A", "B","C","D","E","F","G")
library(ggplot2)
DEPTH<-Data_frame$DEPTH
plot(x=Data_frame$ABIES,y=DEPTH)
str(colnames(Data_frame))
Species <- colnames(Data_frame)
str(colnames(Data_frame))
print(Species)
#for(i in 2:ncol(Data_frame))
ggplot(Data_frame,names=label,aes(x = Type, y = F),color="grey",fill='grey')+
  geom_boxplot(alpha=0.5,outlier.shape = NA)+
  geom_jitter(position=position_jitter(0.1))+
  stat_summary(fun.y = mean, geom = "point",color = "red", size = 2)+
  theme_classic()
for(i in 2:ncol(Data_frame))
        {p<-ggplot(Data_frame,aes_string(x=(colnames(Data_frame)[i]),y=DEPTH))+
        geom_point()+
        theme_classic()
        print(p)}
theme_classic()
print(p)
#geom_boxplot(aes_string(x=(colnames(Data_frame)[i]),y=DEPTH))}

