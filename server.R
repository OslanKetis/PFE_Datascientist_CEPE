#
# This is the user interface for our shiny web app. It is based upon shinydashboard package
# Ideas of vizualisation
#       - Car accident distribution
#       - Region 
#

library(shiny)
library(shinyalert)
library(shinydashboard)
# library(DT)
# library(tigris)
library(leaflet)
library(tidyverse)

# Data 
caracteristiques.2019 <- read.csv2("data/caracteristiques-2019.csv", stringsAsFactors=TRUE)



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
    caracteristiques.2019.select <- reactive({
        caracteristiques.2019 %>% filter(dep == input$dep_fr_map)
        })
    
    output$fr_map <- renderLeaflet({
        # isolate(caracteristiques.2019.select)
        leaflet() %>%
            addTiles() %>%
            addMarkers(lng=(caracteristiques.2019.select())$long, lat=(caracteristiques.2019.select())$lat , popup=(caracteristiques.2019.select())$adr)
            })    
    
}