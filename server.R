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
library(plotly)
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
    output$plot_stats_per_month <- renderPlotly({
        res <- dfresult() %>% mutate(Mois = factor(Mois, levels(lesMois))) 
        fig <- plot_ly(
            type = "scatter",
            x = res$Mois, 
            y = res$`2016`,
            name = '2016',
            mode = "markers+lines",
            line = list(color = '#a799b7')) 
        fig <- fig %>%
            add_trace(
                type = "scatter",
                x = res$Mois, 
                y = res$`2017`,
                name = '2017',
                mode = "markers+lines",
                line = list(color = '#fdca40')) 
        fig <- fig %>%
            add_trace(
                type = "scatter",
                x = res$Mois, 
                y = res$`2018`,
                name = '2018',
                mode = "markers+lines",
                line = list(color = '#fb3640'))  
        fig <- fig %>%
            add_trace(
                type = "scatter",
                x = res$Mois, 
                y = res$`2019`,
                name = '2019',
                mode = "markers+lines",
                line = list(color = '#542e71'))  
        fig <- fig %>% layout(legend = list(orientation = 'v'), title= "Nombre d'accidents par jour pour chaque mois")
        fig
        
    })
    
    # Analyse par heure
    nbParHeures <- reactive({
            carac() %>% 
            mutate(heure = str_sub(hrmn, 1, 2)) %>%
            group_by(heure) %>% 
            summarise(nb = n()) 
    })
    # Rendu 
    output$plot_stats_per_hour <- renderPlotly({
        fig2 <- plot_ly( y = nbParHeures()$nb, x = nbParHeures()$heure, type = "bar", orientation = "v")
        fig2 <- fig2 %>%
            layout(legend = list(orientation = 'v'), title = "Nombre d'accidents par heure pour une année")
        fig2
    })
    
    # PDP
    prediction.62.select <- reactive({
        prediction.62 %>% mutate(var_selected = input$var_selected)
    })
    prediction.44.select <- reactive({ 
        prediction.44 %>% mutate(var_selected = input$var_selected)
    })
    
    # variable continue découpée en classe: Humidite, Temperature, Rafales, rosee(optionnel), Precipitations_24H (plus d'effet pour 62 que 44)
    class.44 <- reactive({
        case_when(
            input$var_selected == "Humidite" ~ cut(prediction.44$Humidite, breaks = 5),
            input$var_selected == "Temperature" ~ cut(prediction.44$Temperature, breaks = 5),
            input$var_selected == "Rafales" ~ cut(prediction.44$Rafales, breaks = 5),
            input$var_selected == "rosee" ~ cut(prediction.44$rosee, breaks = 5),
            input$var_selected == "Precipitations_24H" ~ cut(prediction.44$Precipitations_24H, breaks = 5),
        )
    })
    
        
    output$plot_pdp_box_62 <- renderPlotly({
        fig3 <- plot_ly(y = prediction.62$pred,
                        x = prediction.62[[input$var_selected, exact=FALSE]],
                        color = prediction.62[[input$var_selected, exact=FALSE]],
                        type = "box", orientation = "v")
        fig3 <- fig3 %>%
            layout(legend = list(orientation = 'v'),
                   title = "Partial Dependence Plot - Département 62", 
                   yaxis = list(title = 'Probability of serious injury or death',
                                range = c(0,1)))
        fig3
    })
    output$plot_pdp_box_44 <- renderPlotly({
        fig3 <- plot_ly(y = prediction.44$pred,
                        x = prediction.44[[input$var_selected, exact=FALSE]],
                        color = prediction.44[[input$var_selected, exact=FALSE]],
                        type = "box", orientation = "v")
        fig3 <- fig3 %>%
            layout(legend = list(orientation = 'v'),
                   title = "Partial Dependence Plot - Département 44", 
                   yaxis = list(title = 'Probability of serious injury or death',
                                range = c(0,1)))
        fig3
    })
    
    # A faire en plotly
    output$plot_pdp_grad_62 <- renderPlotly({
        # p <- ggplot(data = prediction.62) +
        #     aes(x= case_when(input$var_selected == "mois" ~ mois,
        #               input$var_selected == "Vitesse_vent" ~ Vitesse_vent,
        #               input$var_selected == "Temperature" ~ Temperature,
        #               input$var_selected == "Humidite" ~ Humidite,
        #               input$var_selected == "Variation_pression" ~ Variation_pression,
        #               input$var_selected == "Rafales" ~ Rafales,
        #               input$var_selected == "rosee" ~ rosee,
        #               input$var_selected == "Precipitations_24H" ~ Precipitations_24H,
        #               TRUE ~ lum), y = pred) +
        #     geom_point() + geom_smooth() + 
        #     ylim(0, 1)
        # fig <- ggplotly(p)
        # fig
        fig4 <- plot_ly(y = prediction.62$pred,
                        x = prediction.62[[input$var_selected, exact=FALSE]],
                        color = prediction.62[[input$var_selected, exact=FALSE]],
                        type = "scatter", orientation = "v")
        fig4 <- fig4 %>%
            layout(legend = list(orientation = 'v'),
                   title = "Partial Dependence Plot - Département 62", 
                   yaxis = list(title = 'Probability of serious injury or death',
                                range = c(0,1)))
        fig4
    })
    output$plot_pdp_grad_44 <- renderPlotly({
        fig4 <- plot_ly(y = prediction.44$pred,
                        x = prediction.44[[input$var_selected, exact=FALSE]],
                        color = prediction.44[[input$var_selected, exact=FALSE]],
                        type = "scatter", orientation = "v")
        fig4 <- fig4 %>%
            layout(legend = list(orientation = 'v'),
                   title = "Partial Dependence Plot - Département 44", 
                   yaxis = list(title = 'Probability of serious injury or death',
                                range = c(0,1)))
        fig4
    })
    
    # output$img_vit_vent <- renderImage({
    #     # Vitesse_vent
    #     filename <- normalizePath(file.path('./pdp',
    #                                         paste('Dep_', input$dep_selected_1,'_Vitesse_vent.png', sep='')))
    #     # Return a list containing the filename and alt text
    #     list(src = filename,
    #          alt = paste("Image number", input$n))
    #     
    # }, deleteFile = FALSE)
    
    # Tab MAP
    fr_stat_select <- reactive({
        fr_stat <- gravity %>% 
            filter(an == input$annee_visu)
        if ("mean_age" == input$crit_visu) {
            fr_stat <- fr_stat %>%  
                group_by(dep) %>% 
                mutate(mean_age = mean(age, na.rm = TRUE)) %>% 
                # mutate(mean_secu_or_not = mean(secu_or_not)) %>% 
                select(mean_age, Total, dep, depFR) %>% 
                slice(1)
        }
        if ("tauxAcc" == input$crit_visu) {
            fr_stat <- fr_stat %>%  
                # filter(classeAge == input$class_Dage) %>% 
                group_by(dep) %>% 
                mutate(tauxAcc = n()*1000/Total) %>% 
                select(tauxAcc, Total, dep, depFR) %>% 
                slice(1)
        }
        if ("tauxAccClasseAge" == input$crit_visu) {
            fr_stat <- fr_stat %>%  
                filter(classeAge == input$class_Dage) %>% 
                group_by(dep) %>% 
                mutate(tauxAccClasseAge = n()*1000/Population_ClassseAge) %>% 
                select(tauxAccClasseAge, Total, Population_ClassseAge, dep, depFR) %>% 
                slice(1)
        }
        if ("tauxGrav" == input$crit_visu) {
            fr_stat <- fr_stat %>%
                group_by(dep) %>% 
                mutate(tauxGrav = mean(as.numeric(grav_or_not))) %>% 
                # filter(!is.na(tauxGrav))%>%
                # mutate(tauxGrav = mean(tauxGrav)) %>% 
                select(tauxGrav, Total, dep, depFR) %>% 
                slice(1)
        }
        if ("tauxGravClasseAge" == input$crit_visu) {
            fr_stat <- fr_stat %>%
                filter(classeAge == input$class_Dage) %>% 
                group_by(dep) %>% 
                mutate(tauxGravClasseAge = mean(as.numeric(grav_or_not))) %>% 
                select(tauxGravClasseAge, Total, dep, depFR) %>% 
                slice(1)
        }
        # Jointure avec le shape
        fr_stat <- france %>% 
            left_join(fr_stat,by=c("iso_3166_2"="depFR")) 
        fr_stat
    })
    
    # Selection du département à afficher sur la carte
    # Source: https://stackoverflow.com/questions/54356383/how-to-fix-label-when-i-hover-mouse-over-map-made-with-tmap


    # output$fr_map_1 <- renderTmap({
    #     tm_basemap("Stamen.Watercolor") +
    #         tm_shape(fr_stat) +
    #         tm_fill(col = "mean_age", palette = "Blues", alpha = 0.8) +
    #         tm_borders("white", lwd = 1)
    # })   
    # 
    # output$fr_map_2 <- renderTmap({
    #     tm_basemap("Stamen.Watercolor") +
    #         tm_shape(fr_stat) +
    #         tm_fill(col = "mean_age", palette = "Blues", alpha = 0.8) +
    #         tm_borders("white", lwd = 1)
    # })   
    
    output$fr_map <- renderTmap({
        tm_basemap("Stamen.Watercolor") +
            tm_shape(fr_stat_select()) +
            tm_fill(col = input$crit_visu, palette = "Blues",
                    alpha = 0.8, id = "name",
                    popup.vars = c("Population département : " = "Total",
                                "Critère : " = input$crit_visu)) +
            tm_borders("black", lwd = 1)
    })

    
}
