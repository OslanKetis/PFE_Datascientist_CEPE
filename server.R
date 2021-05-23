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
library(leaflet)
library(tidyverse)
library(jsonlite)
library(mapview)
library(rnaturalearth)
library(tmap)
tmap_mode(mode = "view")


if(!exists("foo", mode="function")) source("nettoyage.R")

# Nombre d'accidents par jour pour chaque mois
lesMois <- c('Janvier', 'Fevrier', 'Mars', 'Avril', 'Mai', 'Juin', 'Juillet', 'Aout', 'Septembre', 'Octobre', 'Novembre', 'Decembre')
nombreDeJoursParMois <- c(31,29,31,30,31,30,31,31,30,31,30,31)

## TMAP ###
france <- ne_states(country = "France", returnclass = "sf") %>% 
    filter(!name %in% c("Guyane française", "Martinique", "Guadeloupe", "La Réunion", "Mayotte"))

fr_stat <- gravity %>% 
    group_by(dep) %>% 
    mutate(mean_age = mean(age, na.rm = TRUE)) %>% 
    # mutate(mean_secu_or_not = mean(secu_or_not)) %>% 
    select(mean_age, dep) %>% 
    slice(1)
    
fr_stat <- france %>% 
    left_join(fr_stat,by=c("iso_3166_2"="dep")) 


#Server Logic####
server <- function(input, output, session) {
    
    # tab "General statistics"
    # Selection de l'annee selectionnée
    # carac <- reactiveValues()
    # observe({
    #     case_when(input$annee_selected == "2016" ~ (carac$A <- caracteristiques.2016.filtre),
    #               input$annee_selected == "2017" ~ (carac$A <- caracteristiques.2017.filtre),
    #               input$annee_selected == "2018" ~ (carac$A <- caracteristiques.2018.filtre),
    #               input$annee_selected == "2019" ~ (carac$A <- caracteristiques.2019.filtre),
    #               TRUE ~ caracteristiques.2019.filtre
    #         )
    #     })
    
    carac <- reactive({
        caracteristiques.filtre %>% filter(an == input$annee_selected)
    })
    
    # Nombre d'accidents par jour pour chaque mois
    resultat <- reactive({
        nombreDannees = 1
        resultat <- vector()
        for(i in 1:12){
            nombreDaccidents <- nrow(filter(carac(), mois==i))
            nombreDeJoursAuTotal <- nombreDeJoursParMois[i]*nombreDannees
            # if(i==2){
            #     nombreDeJoursAuTotal <- nombreDeJoursAuTotal+3 # 2008, 2012, et 2016 sont bisextiles! Donc un jour de plus en fevrier
            # }
            x <- nombreDaccidents/nombreDannees
            resultat <- c(resultat,x)
        }
        for(i in 1:12){
            resultat[i] <- resultat[i]/nombreDeJoursParMois[i]
        }
        resultat
    })
    # Rendu 
    output$plot_stats_per_month <- renderPlot({
        barplot(height=resultat(), names.arg=lesMois,main="Nombre d'accidents par jour pour chaque mois",las=2)
    })
    
    # Analyse par heure
    nbParHeures <- reactive({
            carac() %>% 
            mutate(hh = str_sub(hrmn, 1, 2)) %>%
            select(hh) %>%
            table()
    })
    # Rendu 
    output$plot_stats_per_hour <- renderPlot({
        barplot(nbParHeures(), main="Nombre d'accidents par heure", xlab="Heure")
    })
    
    # output$general_statistics_by_dep <- renderDT({})
    
    output$state_selected_mini_info <- renderValueBox({})
    
    check_no_country_or_indicator_selected <- reactive({})
    
    observe({})
    
    # Debug
    
    # tab "States comparison"
    # input$state_to_compare_1
    # input$state_to_compare_2
    output$plot_comparison_indicator_between_states <- renderPlot({})
    
    # Selection du département à afficher sur la cart
    caracteristiques.select <- reactive({
        # caracteristiques.filtre %>% filter(an == input$annee_fr_map) %>% filter(dep == input$dep_fr_map)
    })
    
    # output$fr_map <- renderLeaflet({
    #     leaflet() %>%
    #         addTiles() %>%
    #         addMarkers(lng=(caracteristiques.select())$long, lat=(caracteristiques.select())$lat , popup=(paste0(caracteristiques.select()$Num_Acc)))
    #         })    
    
    output$fr_map_1 <- renderTmap({
        tm_basemap("Stamen.Watercolor") +
            tm_shape(fr_stat) +
            tm_fill(col = "mean_age", palette = "Blues", alpha = 0.8) +
            tm_borders("white", lwd = 1)
    })   
    
    output$fr_map_2 <- renderTmap({
        tm_basemap("Stamen.Watercolor") +
            tm_shape(fr_stat) +
            tm_fill(col = "mean_age", palette = "Blues", alpha = 0.8) +
            tm_borders("white", lwd = 1)
    })
    
    # output$fr_map <- renderMapview({
    #     mapview(fr_stat,
    #             zcol = "region",
    #             legend = FALSE,
    #             layer.name = "Departements",
    #             popup = leafpop::popupTable(france, zcol = c("name", "region"), feature.id = FALSE, row.numbers = FALSE)
    #             # popup = leafpop::popupTable(fr_stat, zcol = c("name", "region"), feature.id = FALSE, row.numbers = FALSE)
    #             )
    # })
    
}
