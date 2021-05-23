## Nettoyage des donées pour affichage shiny

#Chargement Data - Filtrage####
caracteristiques.2016 <- read.csv2("data/caracteristiques_2016.csv", sep=',', stringsAsFactors=TRUE)
caracteristiques.2017 <- read.csv2("data/caracteristiques-2017.csv", sep=',', stringsAsFactors=TRUE)
caracteristiques.2018 <- read.csv2("data/caracteristiques-2018.csv", sep=',', stringsAsFactors=TRUE)

caracteristiques.2019 <- read.csv2("data/caracteristiques-2019.csv", stringsAsFactors=TRUE)
lieux.2019 <- read.csv2("data/lieux-2019.csv", sep=";", stringsAsFactors=TRUE)
vehicules.2019 <- read.csv("data/vehicules-2019.csv", sep=";")
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

# Selection des données de l'année. A retravailler pour faire une sélection groupée / fusionnée.
# caracteristiques.filtre <- caracteristiques.2016.filtre

## Visualisation de la gravité par département ####
acc_place_19 <- inner_join(caracteristiques.2019.filtre,lieux.2019,by=c("Num_Acc"="Num_Acc"));

user_car_19<- inner_join(vehicules.2019,usagers.2019,by=c("Num_Acc"="Num_Acc", "id_vehicule" = "id_vehicule", "num_veh" = "num_veh"));

total_19 <- inner_join(acc_place_19,user_car_19,by=c("Num_Acc"="Num_Acc"));

gravity <- total_19%>% 
  # filter(!is.na(grav)) %>%
  mutate(grav_or_not = 
           case_when(
             grav == "2" | grav == "3" ~ "1",
             TRUE ~ "2"
           )
  ) %>% 
  mutate(age = 2019-an_nais) %>% 
  mutate(
    sexe = as.factor(sexe),
    trajet = as.factor(trajet),
    # secu_or_not = as.factor(secu_or_not),
    # secu_level = as.factor(secu_level),
    dep = paste0('FR-', dep)
  ) %>%
  select(grav_or_not, sexe, age, trajet, lat, long, dep)
