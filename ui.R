#
# This is the user interface for our shiny web app. It is based upon shinydashboard package
# Ideas of vizualisation
#       - Car accident distribution
#       - Region 
#
# Sources:
#   https://shiny.rstudio.com/articles/tag-glossary.html
#   https://stackoverflow.com/questions/27982577/how-to-set-a-conditional-panel-to-a-selectinput-in-shiny

library(shiny)
library(shinyalert)
library(shinydashboard)
library(DT)
library(leaflet)
library(tidyverse)
library(mapview)
library(rnaturalearth)
library(rgeos)
library(plotly)

# Data Preparation - Nettoyoge
if(!exists("foo", mode="function")) source("nettoyage.R", encoding = 'UTF-8')

# A réextraire à partir de la population entière.
Departements <- c(66,75) 
Criteres <- c("age","gravite")
Criteres_visu <- c("mean_age","tauxAcc","tauxAccClasseAge", "tauxGrav","tauxGravClasseAge")
Class_Dage <- gravity %>% select(classeAge) %>% arrange(classeAge) %>% distinct 
var_pdp <- names(select(prediction, -id,-pred))

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
            menuItem("Comparaison départements (PDP)", tabName = "tab_dep_comparaison", icon = icon("binoculars")),
            menuItem("Carte d'indicateurs", tabName = "tab_map", icon = icon("globe-europe"))
        )
    ), 
    body = dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "tab_general_statistics", class = "active",
                    h2("Statistiques générales"),
                    div("Les bases de données, extraites du fichier BAAC (Bulletin d'Analyse des Accidents de la Circulation), répertorient l'intégralité des accidents corporels de la circulation routière."),
                    div("Un accident corporel (mortel et non mortel) de la circulation routière relevé par les forces de l’ordre :",
                        tags$ul(tags$li("implique au moins une victime"),
                        tags$li("survient sur une voie publique ou privée, ouverte à la circulation publique"),
                        tags$li("implique au moins un véhicule"))),
                    fluidRow(
                        infoBox("Vision mensuelle", icon = icon("calendar-alt"), width = 6),
                        infoBox("Vision Horaire", icon = icon("hourglass-half"), width = 6),
                        ),
                    fluidRow(
                        box(width = 6,
                            plotlyOutput("plot_stats_per_month")
                        ),
                        box(width = 6,
                            plotlyOutput("plot_stats_per_hour")
                        )
                    ),
                    fluidRow(
                        column(width = 6),
                        box(width = 6, 
                            selectInput(inputId = "annee_selected", label = "Année", choices = c(2019,2018,2017,2016))
                        ),
                        valueBoxOutput(outputId = "annee_selected_mini_info", width = 6)
                    )
            ),
            
            # Second tab content
            tabItem(tabName = 'tab_dep_comparaison', class = "active",
                    fluidRow(
                        infoBox("Département 62", icon = icon("mountain"), width = 6),
                        infoBox("Département 44", icon = icon("wind"), width = 6),
                    ),
                    fluidRow(
                        column(width = 3),
                        box(width = 6, 
                            selectInput(inputId = "var_selected", label = "Variable", choices = var_pdp)
                        ),
                        column(width = 3)
                    ),
                    fluidRow(
                        box(width = 6,
                            plotlyOutput("plot_pdp_box_62")
                        ),
                        box(width = 6,
                            plotlyOutput("plot_pdp_box_44")
                        )
                    ),
                    fluidRow(
                        box(width = 6,
                            plotlyOutput("plot_pdp_grad_62")
                        ),
                        box(width = 6,
                            plotlyOutput("plot_pdp_grad_44")
                        )
                    )
            ),
            
            # Third tab content
            tabItem(tabName = "tab_map", class = "active",
                    fluidRow(
                        box(width = 2,
                            selectInput(inputId = "annee_visu", label = "Année", choices = c(2019,2018,2017,2016)),
                            selectInput(inputId = "crit_visu", label = "Critère de visualisation", choices = Criteres_visu),
                            conditionalPanel(
                                condition = "input.crit_visu === 'tauxAccClasseAge' || input.crit_visu === 'tauxGravClasseAge'",
                                selectInput(inputId = "class_Dage", label = "Classe d'ages ", choices = Class_Dage)),
                            h2("Critères"),
                            div("",
                                tags$ul(tags$li("Mean age: L'age moyen par département"),
                                        tags$li("tauxAcc: Taux d'accident corporel par département (1/1000)"),
                                        tags$li("tauxAccClasseAge: Taux d'accident corporel par département et par classe d'age (1/1000)"),
                                        tags$li("tauxGrav: Taux de gravité moyen des accidents corporel par département"),
                                        tags$li("tauxGravClasseAge: Taux de gravité moyen des accidents corporel par département et par classe d'age")
                                        )),
                        ),
                        box(width = 10,
                            mapviewOutput(outputId = "fr_map", width = "100%", height = 750)
                        )
                    )
            )
        )
    )
)
