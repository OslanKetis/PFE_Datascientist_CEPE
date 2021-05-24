#
# This is the user interface for our shiny web app. It is based upon shinydashboard package
# Ideas of vizualisation
#       - Car accident distribution
#       - Region 
#

library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(tidyverse)
library(jsonlite)
library(mapview)
library(rnaturalearth)
library(tmap)
library(ggplot2)
tmap_mode(mode = "view")


if(!exists("foo", mode="function")) source("nettoyage.R", encoding = 'UTF-8')

# Nombre d'accidents par jour pour chaque mois
lesMois <- factor(levels = c('Janvier', 'Fevrier', 'Mars', 'Avril', 'Mai', 'Juin', 'Juillet', 'Aout', 'Septembre', 'Octobre', 'Novembre', 'Decembre'))
nombreDeJoursParMois <- c(31,29,31,30,31,30,31,31,30,31,30,31)
annees <- c(2019,2018,2017,2016)

## TMAP ###
france <- ne_states(country = "France", returnclass = "sf") %>% 
    filter(!name %in% c("Guyane française", "Martinique", "Guadeloupe", "La Réunion", "Mayotte"))


#Server Logic####
server <- function(input, output, session) {
    
    # tab "General statistics"
    carac <- reactive({
        caracteristiques.filtre %>% filter(an == input$annee_selected)
    })
    
    # Nombre d'accidents par jour pour chaque mois - sur toute la période
    dfresult <- reactive({
        nombreDannees = 1
        df <- data.frame(row.names = levels(lesMois))
        for (i in annees){
            resultat <- c()
            for(j in 1:12){
                nombreDaccidents <- nrow(filter(caracteristiques.filtre, an==i, mois==j))
                nombreDeJoursAuTotal <- nombreDeJoursParMois[j]*nombreDannees
                if(i==2016){
                    nombreDeJoursAuTotal <- nombreDeJoursAuTotal+3 # 2008, 2012, et 2016 sont bisextiles! Donc un jour de plus en fevrier
                }
                x <- nombreDaccidents/(nombreDannees*nombreDeJoursParMois[j])
                resultat[j] <- x
            }
            df <- cbind(df, i=resultat)
        }
        colnames(df) <- annees
        df <- cbind(df, Mois = levels(lesMois))
        df
    })
    # Rendu 
    output$plot_stats_per_month <- renderPlot({
        # barplot(height=resultat(), names.arg=lesMois,main="Nombre d'accidents par jour pour chaque mois",las=2)
        dfresult() %>% mutate(Mois = factor(Mois, levels(lesMois))) %>% ggplot(aes(lesMois))  + 
            geom_point(aes(x=Mois, y=`2016`, group = 1),shape=16, color="blue", size=4) +
            geom_line(aes(x=Mois, y=`2016`, group = 1), color="blue") +
            geom_point(aes(x=Mois, y=`2017`, group = 1), shape=16, color="green", size=4) +
            geom_line(aes(x=Mois, y=`2017`, group = 1), color="green") +
            geom_point(aes(x=Mois, y=`2018`, group = 1), shape=16, color="orange", size=4) +
            geom_line(aes(x=Mois, y=`2018`, group = 1), color="orange") +
            geom_point(aes(x=Mois, y=`2019`, group = 1), shape=16, color="red", size=4) +
            geom_line(aes(x=Mois, y=`2019`, group = 1), color="red") +
            theme_classic() +
            labs(x="Mois", y="Nombre d'accidents") +
            ggtitle("Nombre d'accidents par jour pour chaque mois")
        
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
    
    
    # Tab MAP
    fr_stat_select <- reactive({
        fr_stat <- gravity %>% 
            filter(an == input$annee_visu)
        if ("mean_age" == input$crit_visu) {
            fr_stat <- fr_stat %>%  
                group_by(dep) %>% 
                mutate(mean_age = mean(age, na.rm = TRUE)) %>% 
                # mutate(mean_secu_or_not = mean(secu_or_not)) %>% 
                select(mean_age, dep, depFR) %>% 
                slice(1)
        }
        if ("tauxAcc" == input$crit_visu) {
            fr_stat <- fr_stat %>%  
                filter(classeAge == input$class_Dage) %>% 
                group_by(dep) %>% 
                mutate(tauxAcc = n()*1000/Population) %>% 
                select(tauxAcc, dep, depFR) %>% 
                slice(1)
        }
        # Jointure avec le shape
        fr_stat <- france %>% 
            left_join(fr_stat,by=c("iso_3166_2"="depFR")) 
        fr_stat
    })
    
    # output$state_selected_mini_info <- renderValueBox({})
    # 
    # check_no_country_or_indicator_selected <- reactive({})
    # 
    # observe({})
    
    # Debug
    
    # tab "States comparison"
    # output$plot_comparison_indicator_between_states <- renderPlot({})
    
    # Selection du département à afficher sur la carte


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
    
    output$fr_map <- renderTmap({
        tm_basemap("Stamen.Watercolor") +
            tm_shape(fr_stat_select()) +
            tm_fill(col = input$crit_visu, palette = "Blues", alpha = 0.8) +
            tm_borders("black", lwd = 1)
    })

    
}
