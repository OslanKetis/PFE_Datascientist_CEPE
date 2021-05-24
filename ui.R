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
if(!exists("foo", mode="function")) source("nettoyage.R", encoding = 'UTF-8')

# A réextraire à partir de la population entière.
Departements <- caracteristiques.2019 %>% select(dep) %>% distinct 
Criteres <- c("age","gravite")
Criteres_visu <- c("mean_age","tauxAcc")
Class_Dage <- gravity %>% select(classeAge) %>% arrange(classeAge) %>% distinct 

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
            menuItem("Carte d'indicateurs", tabName = "tab_map", icon = icon("globe-europe"))
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
                            selectInput(inputId = "crit_comp", label = "Critère de comparaison", choices = Criteres)
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
                        box(width = 12,
                            selectInput(inputId = "annee_visu", label = "Année", choices = c(2019,2018,2017,2016)),
                            selectInput(inputId = "crit_visu", label = "Critère de visualisation", choices = Criteres_visu),
                            selectInput(inputId = "class_Dage", label = "Classe d'ages ", choices = Class_Dage)
                        )
                    ),
                    fluidRow(
                        box(width = 12,
                            mapviewOutput(outputId = "fr_map", width = "100%", height = 750)
                        )
                    )
            )
        )
    )
)
