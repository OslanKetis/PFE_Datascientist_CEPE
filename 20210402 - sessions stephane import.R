## PFE

# librairy
library(tidyverse)

#####   Importation des données
#----------

#fonction import csv
importData <- function(chemin,type_col){
  donnees<- read_csv(file = chemin,
                     col_names = TRUE,
                     col_types = type_col,
                     )
  donnees<-donnees%>%mutate(fichierSource=rep(chemin,donnees%>%summarise(n())))
  return(donnees)
}

# import des caracteristiques
chemin <-paste0("D:/Users/slauzevis/OneDrive - LISI/Documents/Rstudio/PFE/Data_dowloaded/Caracteristiques/caracteristiques_",rep(2014:2019),".csv")
type_col<-cols(
  Num_Acc=col_double(),
  an=col_factor(),
  mois=col_factor(),
  jour=col_factor(), 
  hrmn=col_guess(),
  lum=col_factor(),
  agg=col_factor(),
  int=col_factor(),
  atm=col_factor(),
  col=col_factor(),
  com=col_factor(),
  adr=col_character(),
  gps=col_character(),
  lat=col_character(),
  long=col_character(),
  dep=col_factor())

#import 2014 à 2018
caracteristiques<-importData(chemin[1],type_col)
for(i in 2:5) {
  dontemp<-importData(chemin[i],type_col)
  caracteristiques<-bind_rows(caracteristiques,dontemp)
  print(chemin[i])
  print(dontemp%>%summarise(n()))
}

# import 2019 ( format different)
dontemp2<-read_csv2(file = chemin[6],
                    col_types = cols(
                                  Num_Acc=col_double(),
                                  jour=col_factor(),
                                  mois=col_factor(),
                                  an=col_factor(),
                                  hrmn=col_guess(),
                                  lum=col_factor(),
                                  dep=col_factor(),
                                  com=col_factor(),
                                  agg=col_factor(),
                                  int=col_factor(),
                                  atm=col_factor(),
                                  col=col_factor(),
                                  adr=col_character(),
                                  lat=col_character(),
                                  long=col_character()))

caracteristiques <- merge(x = caracteristiques,y = dontemp2,all.x = TRUE,all.y = TRUE)


#  il faut corriger les données genre les heures. les departement. l'idée est de les rendre homogene.
