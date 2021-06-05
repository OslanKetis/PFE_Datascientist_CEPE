## Nettoyage des donées pour affichage shiny
library(tidyverse)
library(readxl)
library(forcats)

#Chargement Data - Filtrage####
caracteristiques.2016 <- read.csv2("data/caracteristiques_2016.csv", sep=',', stringsAsFactors=TRUE)
caracteristiques.2017 <- read.csv2("data/caracteristiques-2017.csv", sep=',', stringsAsFactors=TRUE)
caracteristiques.2018 <- read.csv2("data/caracteristiques-2018.csv", sep=',', stringsAsFactors=TRUE)
caracteristiques.2019 <- read.csv2("data/caracteristiques-2019.csv", stringsAsFactors=TRUE)

lieux.2016 <- read.csv2("data/lieux_2016.csv", sep=',')
lieux.2017 <- read.csv2("data/lieux-2017.csv", sep=',')
lieux.2018 <- read.csv2("data/lieux-2018.csv", sep=',')
lieux.2019 <- read.csv2("data/lieux-2019.csv", sep=";")

vehicules.2016 <- read.csv("data/vehicules_2016.csv", sep=",")
vehicules.2017 <- read.csv("data/vehicules-2017.csv", sep=",")
vehicules.2018 <- read.csv("data/vehicules-2018.csv", sep=",")
vehicules.2019 <- read.csv("data/vehicules-2019.csv", sep=";")

usagers.2016 <- read.csv("data/usagers_2016.csv", sep=",")
usagers.2017 <- read.csv("data/usagers-2017.csv", sep=",")
usagers.2018 <- read.csv("data/usagers-2018.csv", sep=",")
usagers.2019 <- read.csv("data/usagers-2019.csv", sep=";")

# Départements
filtre_dep_lat_long_an <- function(tbl1) {
  tbl1 <- tbl1 %>% mutate(dep = case_when(dep == 201 ~ "2A",
                                          dep == 202 ~ "2B",
                                          dep == 971 ~ "971",
                                          dep == 972 ~ "972",
                                          dep == 973 ~ "973",
                                          dep == 975 ~ "975",
                                          dep == 974 ~ "974",
                                          dep == 976 ~ "976",
                                          dep == 977 ~ "977",
                                          dep == 978 ~ "978",
                                          dep == 987 ~ "987",
                                          dep == 986 ~ "986",
                                          dep == 988 ~ "988",
                                          TRUE ~ str_pad(dep/10, 2, pad = "0")))
  tbl1 <- tbl1 %>% mutate(lat = lat / 100000) %>% 
    mutate(long = long / 100000) %>%
    mutate(an = as.numeric(paste0('20',an)))
}

filtre_invers_signe <- function(tbl1) {
  # Inversion des signes pour lattitudes et longitudes pour les départements 971, 972, 973, 974, 976
  tbl1 <- tbl1 %>% mutate(lat = case_when(dep %in% c(976,974) ~ -lat, TRUE ~ lat))
  tbl1 <- tbl1 %>% mutate(long = case_when(dep %in% c(972,973,971) ~ -long, TRUE ~ long))
}

#Annee 2016####
caracteristiques.2016 <- filtre_dep_lat_long_an(caracteristiques.2016)
caracteristiques.2016 <- filtre_invers_signe(caracteristiques.2016)

# caracteristiques.2016 <- mutate (caracteristiques.2016, if (length(dep) == 1) {dep = paste0('0',dep)}

# Erreurs de saisie sur lat=0 et long=0
caracteristiques.2016 <- caracteristiques.2016 %>% filter(!(lat == 0 & long == 0))
caracteristiques.2016.filtre <- caracteristiques.2016 %>% filter(!(lat == 0))


#Annee 2017####
caracteristiques.2017 <- filtre_dep_lat_long_an(caracteristiques.2017)
caracteristiques.2017 <- filtre_invers_signe(caracteristiques.2017)

# Anomalie dans le département 92. Des coordonnées arbitraires
caracteristiques.2017 <- caracteristiques.2017 %>% filter(!(dep == 92 & long == 2.2 & lat == 48))

# Valeur de GPS non concordantes au département
carac.2017.erreur <- read.csv2("data/coord_erreur_2017.csv", sep=';', stringsAsFactors=TRUE)
caracteristiques.2017.filtre <- caracteristiques.2017 %>% filter(!Num_Acc %in% carac.2017.erreur$coordonnes)


#Annee 2018####
caracteristiques.2018 <- filtre_dep_lat_long_an(caracteristiques.2018)
caracteristiques.2018 <- filtre_invers_signe(caracteristiques.2018)

# Tous les accidents du département 972 ayant -60 en long sont éronnées au niveau de leur coordonnées GPS.
caracteristiques.2018 <- caracteristiques.2018 %>% filter(!(dep == 972 & long == 60))

# Valeur de GPS non concordantes au département
carac.2018.erreur <- read.csv2("data/coord_erreur_2018.csv", sep=';', stringsAsFactors=TRUE)
caracteristiques.2018.filtre <- caracteristiques.2018 %>% filter(!Num_Acc %in% carac.2018.erreur$coordonnes)

# write.csv2(caracteristiques.2018.filtre, sep=';', file = "data/caracteristiques-2018-filtre.csv", row.names = FALSE)
# test <- read.csv2("data/caracteristiques-2018-filtre.csv", sep=';', stringsAsFactors=TRUE)

#Annee 2019####
caracteristiques.2019$dep <- fct_recode(caracteristiques.2019$dep,
                                        "01"= "1",
                                        "02"= "2",
                                        "03"= "3",
                                        "04"= "4",
                                        "05"= "5",
                                        "06"= "6",
                                        "07"= "7",
                                        "08"= "8",
                                        "09"= "9",)
# Nettoyage des accidents dont les coordonnées GPS ne correspondent pas. Annee 2019
carac.erronnes <- c(201900033874, 201900035540, 201900035541, 201900007105, 201900058723, 201900017468, 201900005927, 201900033236, 201900006562
                    , 201900056421, 201900000632, 201900057241, 201900048863, 201900008746, 201900056494, 201900047454, 201900021441, 201900025903
                    ,201900020406)
caracteristiques.2019.filtre <- caracteristiques.2019 %>% filter(!Num_Acc %in% carac.erronnes)


# Aggregation####
##hrmn####
to_hrmn<- function(tbl1){
  tbl1$hrmn <- as.character(sprintf("%04d", tbl1$hrmn))
  tbl1$hrmn <- paste0(substr(tbl1$hrmn,1,2),":",substr(tbl1$hrmn,3,4))
  tbl1$hrmn <- factor(tbl1$hrmn)
  return(tbl1)
}
caracteristiques.2016.filtre <- to_hrmn(caracteristiques.2016.filtre)
caracteristiques.2017.filtre <- to_hrmn(caracteristiques.2017.filtre)
caracteristiques.2018.filtre <- to_hrmn(caracteristiques.2018.filtre)

##dep####
caracteristiques.2016.filtre$dep <- factor(caracteristiques.2016.filtre$dep)
caracteristiques.2017.filtre$dep <- factor(caracteristiques.2017.filtre$dep)
caracteristiques.2016.filtre$dep <- factor(caracteristiques.2016.filtre$dep)
caracteristiques.2017.filtre$dep <- factor(caracteristiques.2017.filtre$dep)
caracteristiques.2018.filtre$dep <- factor(caracteristiques.2018.filtre$dep)
##adr####
caracteristiques.2016.filtre$adr <- as.character(caracteristiques.2016.filtre$adr)
caracteristiques.2017.filtre$adr <- as.character(caracteristiques.2017.filtre$adr)
caracteristiques.2018.filtre$adr <- as.character(caracteristiques.2018.filtre$adr)
caracteristiques.2019.filtre$adr <- as.character(caracteristiques.2019.filtre$adr)
##com####
caracteristiques.2019.filtre$com <- as.numeric(caracteristiques.2019.filtre$com)


##result####
caracteristiques.filtre <- bind_rows(caracteristiques.2016.filtre, caracteristiques.2017.filtre, caracteristiques.2018.filtre, caracteristiques.2019.filtre)


# Aggregation des tables

acc_place_16 <- inner_join(caracteristiques.2016.filtre, lieux.2016, by=c("Num_Acc"="Num_Acc"));
user_car_16<- inner_join(vehicules.2016, usagers.2016, by=c("Num_Acc"="Num_Acc", "num_veh" = "num_veh"));
total_16 <- inner_join(acc_place_16, user_car_16, by=c("Num_Acc"="Num_Acc"));

acc_place_17 <- inner_join(caracteristiques.2017.filtre, lieux.2017, by=c("Num_Acc"="Num_Acc"));
user_car_17<- inner_join(vehicules.2017, usagers.2017, by=c("Num_Acc"="Num_Acc", "num_veh" = "num_veh"));
total_17 <- inner_join(acc_place_17, user_car_17, by=c("Num_Acc"="Num_Acc"));

acc_place_18 <- inner_join(caracteristiques.2018.filtre, lieux.2018, by=c("Num_Acc"="Num_Acc"));
user_car_18<- inner_join(vehicules.2018, usagers.2018, by=c("Num_Acc"="Num_Acc", "num_veh" = "num_veh"));
total_18 <- inner_join(acc_place_18, user_car_18, by=c("Num_Acc"="Num_Acc"));

acc_place_19 <- inner_join(caracteristiques.2019.filtre,lieux.2019,by=c("Num_Acc"="Num_Acc"));
user_car_19<- inner_join(vehicules.2019,usagers.2019,by=c("Num_Acc"="Num_Acc", "id_vehicule" = "id_vehicule", "num_veh" = "num_veh"));
total_19 <- inner_join(acc_place_19,user_car_19,by=c("Num_Acc"="Num_Acc"));


## INSEE :import des données sur la pop par dep ####
Plage <- c("C5:W107","X5:AR107","AS5:BM107","A5:A107")
Spreadsheet <- c("2019","2019","2019","2019")
Genre <- c("Ensemble","Hommes","Femmes","Departement")
Pop_lst <- map2(.x = Spreadsheet,.y = Plage,~read_xlsx(path = "data/estim-pop-dep-sexe-aq-1975-2021.xlsx",
                                                     sheet = .x,
                                                     range = .y))
names(x = Pop_lst) = Genre
# formating des données pour repasser en facteurs 
Sexe <- factor(x = c("Tot",1,2),levels = c("Tot",1,2), labels = c("Tot","1","2")) 
Dep_levels <- unique(as_vector(Pop_lst$Departement))
ClassAge_levels <- c("0 à 4 ans","5 à 9 ans","10 à 14 ans","15 à 19 ans","20 à 24 ans",
                   "25 à 29 ans","30 à 34 ans","35 à 39 ans","40 à 44 ans","45 à 49 ans",
                   "50 à 54 ans","55 à 59 ans","60 à 64 ans","65 à 69 ans","70 à 74 ans",
                   "75 à 79 ans","80 à 84 ans","85 à 89 ans","90 à 94 ans","95 ans et plus")
ClasseDage <- c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,95)
# boucle pour repasser des listes en 1 seul tibble
Departements <- factor(pluck(.x = Pop_lst, "Departement", "...1"), levels = Dep_levels)
Pop_tbl <- tibble()
rslt <- tibble()
for (i in 1:3){
  rslt<-tibble(Pop_lst[[i]]) %>%
    add_column("dep"= Departements,
               "sexe" = Sexe[i])
  Pop_tbl <- bind_rows(Pop_tbl, rslt)
}
#reshape Data via pivot_longer.
Pop_tbl <- Pop_tbl%>%pivot_longer(cols = 1:20,names_to = "ClasseAge",values_to = "Population_ClassseAge")
Pop_tbl <- Pop_tbl%>%mutate("ClasseAge"=factor(x = ClasseAge,levels = ClassAge_levels))
Pop_tbl <- Pop_tbl%>%filter(sexe == "Tot") %>% select(-sexe)

# release of variable
rm(rslt)  
rm(i)
rm(Plage)
rm(Spreadsheet)
rm(Genre)
rm(Departements)


## Visualisation de la gravité et autres critères par département ####

filtrage_gravite <- function(tbl1, annee) {
  tbl1%>% 
    # filter(!is.na(grav)) %>%
    mutate(grav_or_not = 
             case_when(
               grav == "2" | grav == "3" ~ "2",
               TRUE ~ "1"
             )
    ) %>% 
    mutate(age = annee-an_nais) %>% 
    mutate(
      sexe = as.factor(sexe),
      trajet = as.factor(trajet),
      # secu_or_not = as.factor(secu_or_not),
      # secu_level = as.factor(secu_level),
      depFR = paste0('FR-', dep)
    ) %>%
    mutate(classeAge = cut(age,breaks = ClasseDage, labels = ClassAge_levels)) %>% 
    select(an, grav, grav_or_not, sexe, age, classeAge, trajet, lat, long, dep, depFR) %>% 
    inner_join(Pop_tbl, by=c("dep"="dep", "classeAge" = "ClasseAge" ))
}

gravity_16 <- filtrage_gravite(total_16,2016)
gravity_17 <- filtrage_gravite(total_17,2017)
gravity_18 <- filtrage_gravite(total_18,2018)
gravity_19 <- filtrage_gravite(total_19,2019)

gravity <- bind_rows(gravity_16, gravity_17, gravity_18, gravity_19)