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
library(mapview)
library(rnaturalearth)
library(rgeos)

# Data Preparation - Nettoyoge
if(!exists("foo", mode="function")) source("nettoyage.R")

# A réextraire à partir de la population entière.
Departements <- caracteristiques.2019 %>% select(dep) %>% distinct 
Criteres <- c("age","gravite")

mapviewOptions(
    basemaps = c("Esri.WorldShadedRelief", "OpenStreetMap.DE"),
    raster.palette = grey.colors,
    vector.palette = colorRampPalette(c("snow", "cornflowerblue", "grey10")),
    na.color = "magenta",
    layers.control.pos = "topright")


# Define UI for application that draws a histogram
ui <- dashboardPage(
    header = dashboardHeader(title = 'Accidents corporels 2017 - 2019'), 
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem("Statistiques générales", tabName = "tab_general_statistics", icon = icon("car-crash")),
            menuItem("Comparaison département", tabName = "tab_dep_comparaison", icon = icon("binoculars")),
            menuItem("Carte", tabName = "tab_map", icon = icon("globe-europe"))
        )
    ), 
    body = dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "tab_general_statistics", class = "active",
                    fluidRow(
                        box(width = 6, background = 'blue',
                            selectInput(inputId = "annee_selected", label = "Année", choices = c(2019,2018,2017,2016))
                        ),
                        valueBoxOutput(outputId = "annee_selected_mini_info", width = 6)
                    ),
                    fluidRow(
                        box(width = 6,
                            plotOutput("plot_stats_per_month")
                        ),
                        box(width = 6,
                            plotOutput("plot_stats_per_hour")
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = 'tab_dep_comparaison', class = "active",
                    fluidRow(
                        box(
                            selectInput(inputId = "dep_selected_1", label = "Département 1", choices = Departements)
                        ),
                        box(
                            selectInput(inputId = "dep_selected_2", label = "Département 2", choices = Departements)
                        )
                    ),
                    fluidRow(
                        box(
                            selectInput(inputId = "critere", label = "Critère de comparaison", choices = Criteres)
                        )
                    ),
                    fluidRow(
                        box(
                            # selectInput(inputId = "annee_fr_map", label = "Année", choices = c(2016,2017,2018,2019)),
                            # selectInput(inputId = "dep_fr_map", label = "Département", choices = Departements),
                            mapviewOutput(outputId = "fr_map_1", width = "100%", height = 600)
                        ),
                        box(
                            # selectInput(inputId = "annee_fr_map", label = "Année", choices = c(2016,2017,2018,2019)),
                            # selectInput(inputId = "dep_fr_map", label = "Département", choices = Departements),
                            mapviewOutput(outputId = "fr_map_2", width = "100%", height = 600)
                        )
                    )
            ),
            
            # Third tab content
            tabItem(tabName = "tab_map", class = "active",
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
            )
        )
    )
)
