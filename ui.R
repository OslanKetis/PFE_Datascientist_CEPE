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


# Data Preparation 


# load .csv dataset
caracteristiques.2019 <- read.csv2("data/caracteristiques-2019.csv", stringsAsFactors=TRUE)

# lieux.2019 <- read.csv2("data/lieux-2019.csv", sep=";", stringsAsFactors=TRUE)

# vehicules.2019 <- read.csv("data/vehicules-2019.csv", sep=";")

# usagers.2019 <- read.csv("data/usagers-2019.csv", sep=";")

# A réextraire à partir de la population entière.
Departements <- caracteristiques.2019 %>% select(dep) %>% distinct 



# Define UI for application that draws a histogram
ui <- dashboardPage(
    header = dashboardHeader(title = 'Etude Accidents de la route corporel 2017 - 2019'), 
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem("Statistiques générales", tabName = "tab_general_statistics", icon = icon("car-crash")),
            menuItem("Comparaison département", tabName = "tab_dep_comparaison", icon = icon("binoculars")),
            menuItem("Carte", tabName = "tab_map", icon = icon("globe-europe"))
        )
    ), 
    body = dashboardBody(
        tabItems(
            tabItem(tabName = "tab_general_statistics", class = "active",
                    fluidRow(
                        box(width = 6, background = 'blue',
                            selectInput(inputId = "dep_selected", label = "Département sélectionné", choices = Departements)
                        ),
                        valueBoxOutput(outputId = "dep_selected_mini_info", width = 6)
                    ),
                    fluidRow(
                        box(width = 12,
                            DTOutput("general_statistics_by_dep")
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "tab_dep_comparaison", class = "active",
                    fluidRow(
                        useShinyalert(),
                        box(
                            selectizeInput(inputId = "dep_to_compare", options = list(maxItems = 4), label = "Départements (max 4)", choices = Departements),
                            selectizeInput(inputId = "indicators_to_compare", options = list(maxItems = 5), label = "Indicateurs (max 5)", choices = Departements),
                            helpText("Note: start typing in to get suggestions.\nTry to break the app by selecting no state or indicator, see what happens :)")
                        ),
                        box(
                            checkboxInput(inputId = "dont_use_facets_for_states", label = "Overlay the time series ?", value = FALSE)
                        ), 
                        plotOutput(outputId = "plot_comparison_indicator_between_states")
                    )
            ),
            
            tabItem(tabName = 'tab_map', class = "active",
                    selectInput(inputId = "annee_fr_map", label = "Année", choices = c(2016,2017,2018,2019)),
                    selectInput(inputId = "dep_fr_map", label = "Département", choices = Departements),
                    leafletOutput(outputId = "fr_map")
            )
        )
    )
)
