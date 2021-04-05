#
# This is the user interface for our shiny web app. It is based upon shinydashboard package
# Ideas of vizualisation
#       - Car accident distribution
#       - Region 
#

library(shiny)
library(shinyalert)
library(shinydashboard)
library(DT)
library(tigris)
library(leaflet)
library(tidyverse)


#Annee 2018####
# Data 
caracteristiques.2018 <- read.csv2("data/caracteristiques-2018.csv", sep=',', stringsAsFactors=TRUE)
# Départements
caracteristiques.2018 <- caracteristiques.2018 %>% mutate(dep = case_when(dep == 201 ~ "2A",
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
                                                        TRUE ~ as.character(dep/10)))
# Longitude, Latitude, année
caracteristiques.2018 <- caracteristiques.2018 %>%
    mutate(lat = lat / 100000) %>% 
    mutate(long = long / 100000) %>%
    mutate(an = 2018)
# Inversion des signes pour lattitudes et longitudes pour les départements 971, 972, 973, 974, 976
caracteristiques.2018 <- caracteristiques.2018 %>% mutate(lat = case_when(dep %in% c(976,974) ~ -lat, TRUE ~ lat))
caracteristiques.2018 <- caracteristiques.2018 %>% mutate(long = case_when(dep %in% c(972,973,971) ~ -long, TRUE ~ long))

# Valeur de GPS non concordantes au département
carac.2018.erreur <- read.csv2("data/coord_erreur_2018.csv", sep=';', stringsAsFactors=TRUE)
caracteristiques.2018.filtre <- caracteristiques.2018 %>% filter(!Num_Acc %in% carac.2018.erreur$coordonnes)


#Annee 2019####

caracteristiques.2019 <- read.csv2("data/caracteristiques-2019.csv", stringsAsFactors=TRUE)
# Nettoyage des accidents dont les coordonnées GPS ne correspondent pas. Annee 2019
carac.erronnes <- c(201900033874, 201900035540, 201900035541, 201900007105, 201900058723, 201900017468, 201900005927, 201900033236, 201900006562
                    , 201900056421, 201900000632, 201900057241, 201900048863, 201900008746, 201900056494, 201900047454, 201900021441, 201900025903
                    ,201900020406)

# Selection des données
# caracteristiques.2019.filtre <- caracteristiques.2018
caracteristiques.2019.filtre <- caracteristiques.2019 %>% filter(!Num_Acc %in% carac.erronnes)

# Server Logic

server <- function(input, output, session) {
    
    # tab "General statistics"
    output$general_statistics_by_dep <- renderDT({})
    
    output$state_selected_mini_info <- renderValueBox({})
    
    check_no_country_or_indicator_selected <- reactive({})
    
    observe({})
    
    # Debug
    
    # tab "States comparison"
    # input$state_to_compare_1
    # input$state_to_compare_2
    output$plot_comparison_indicator_between_states <- renderPlot({})
    
    # Selection du département à afficher sur la cart
    caracteristiques.2018.select <- reactive({
        caracteristiques.2018.filtre %>% filter(dep == input$dep_fr_map)
    })
    caracteristiques.2019.select <- reactive({
        caracteristiques.2019.filtre %>% filter(dep == input$dep_fr_map)
        })
    
    output$fr_map <- renderLeaflet({
        # isolate(caracteristiques.2019.select)
        leaflet() %>%
            addTiles() %>%
            addMarkers(lng=(caracteristiques.2018.select())$long, lat=(caracteristiques.2018.select())$lat , popup=(paste0(caracteristiques.2018.select()$Num_Acc)))
            })    
    
}