#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  ## ONGLET CARTE
  
  
  # Permet de faire correspondre le nom simplifié du tournois avec son id
  # ex : Starladder Major Berlin 2019 est simplifié pour l'affichage en Berlin 20198
  formatted_event = reactive({
    majorID[match(input$plot_event,majorName)]
  })
  
  #Permet d'afficher le fond de carte
  output$mymap <- renderLeaflet({
    basemap
  })
  
  # Permet de filtrer le dataframe pour obtenir les datas correspond au tournois souhaité
  reactive_db = reactive({
    pays_major %>% filter(event_id == formatted_event())
  })
  
  # Permet de générer les contours des pays en fonction des données rentrées par l'utilisateur
  reactive_polygons = reactive({
    worldcountry[worldcountry$ADM0_A3 %in% reactive_db()$alpha3, ]
  })
  
  
  # Génération des labels (nom du pays et nombre de représentant)
  reactive_label = reactive({
    sprintf(
      "<strong>%s</strong><br/>%g",
      reactive_db()$country, reactive_db()$nbRep
    ) %>% lapply(htmltools::HTML)
    
  })
  
  # A l'aider des expressions précédentes, on peut générer la nouvelle carte lorsque l'application
  # détecte un changement dans le tournois souhaité
  observeEvent(input$plot_event, {
    
    leafletProxy("mymap") %>%
      clearShapes() %>%
      addPolygons(data =  reactive_polygons(),
                  stroke = FALSE,
                  smoothFactor = 0.2,
                  fillOpacity = 0.5,
                  fillColor = ~cv_pal(reactive_db()$nbRep),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = reactive_label(),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      )
    
    
    
  })
  
  
  ### Onglet histogramme
  output$firstHistoPlot <- renderPlotly({
    p_histo_r
  })
  
  output$secondHistoPlot <- renderPlotly({
    p_histo_bis_r
  })
  
  
  ### Onglet kill-différence
  output$kd_plot <- renderPlotly({
    p_kd
  })
  
  ### Onglet Joueur
  
  # Similaire à l'expression précédente
  formatted_event_2 = reactive({
    majorID[match(input$event,majorName)]
  })
  
  # Permet de récupérer les statistiques souhaité en fonction de l'équipe renseigné par l'utilisateur
  reactive_db_team = reactive({
    df_stats_long %>% filter(team == input$team & event_id == formatted_event_2() & stat_name %in% c("ratio_kd","nbAssists","kast","mean_rating","mean_adr","nbFlashAssists","tauxHs"))
  })
  
  # Permet d'isoler les mois pour lesquels nous avons des statistiques pour le joueur voulu
  # Par exemple les premières données pour " ZywOo " sont en 2018 contre 2016 pour " s1mple "
  reactive_mois_rating = reactive({
    unique((df_mean_rating %>% filter(player_name == input$player & !is.na(rating_mean)))$mois)
  })
  
  # Permet d'isoler tous les ratings mais seulement pour les mois obtenus avec l'expression précédentes
  # Ces données seront utilisés pour tracer le boxplot
  reactive_db_rating_boxplot = reactive({
    df_mean_rating %>% filter (mois %in% reactive_mois_rating() )
  })
  
  # Permet d'isoler tous les ratings du joueur voulu
  reactive_db_rating_player = reactive({
    df_mean_rating %>% filter(player_name %in% c(input$player) & mois %in% reactive_mois_rating())
  })
  
  # Permet d'isoler toutes les statistiques en fonction du joueur, du tournoi et de l'équipe (nécessaire car certains joueurs ont le même pseudo)
  reactive_db_stat  = reactive({
    df_stats_long %>% filter(player_name == input$player & event_id == formatted_event_2() & team == input$team)
  })
  
  # Permet de venir mettre à jour les données textuelles
  output$ruban <- renderText({ 
    input$submitButton # Permet d'ajouter une dépendance au bouton
    
    # La ligne suivante ne sera ainsi éxécuté seulement si le bouton est cliqué grâce à la fonction isolate
    stats <- isolate(reactive_db_stat()) 
    
    # Si on a trouvé une correspondance pour le joueur et le tournoi souhaité, nous allons afficher ses statistiques
    # Sinon nous affichons qu'aucun joueur n'a été trouvé
    if (dim(stats)[1]>0) {
      output$kills_value <- renderText({ paste("Nombre de kills : ",(stats %>% filter (stat_name == "nbKills"))$value ) })
      output$deaths_value <- renderText({ paste("Nombre de deaths : ",(stats %>% filter (stat_name == "nbDeaths"))$value)})
      output$kd_value <- renderText({ paste("Différence : ",(stats %>% filter (stat_name == "kdDiff"))$value)})
      output$ratio_value <- renderText({ paste("Ratio : ",round((stats %>% filter (stat_name == "ratio_kd"))$value /100,digits=2))})
      output$assists_value <- renderText({ paste("Nombre d'Assists :",(stats %>% filter (stat_name == "nbAssists"))$value)})
      output$hs_value <- renderText({ paste("Taux de Headshot :",(stats %>% filter (stat_name == "tauxHs"))$value," %") })
      output$fassists_value <- renderText({ paste("Nombre de Flash Assists :",(stats %>% filter (stat_name == "nbFlashAssists"))$value)})
      output$adr_value <- renderText({ paste("ADR Moyen:",(stats %>% filter (stat_name == "mean_adr"))$value)})
      output$rating_value <- renderText({ paste("Rating Moyen : ",round((stats %>% filter (stat_name == "mean_rating"))$value /100,digits=2))})
      output$kast_value <- renderText({ paste("KAST :",(stats %>% filter (stat_name == "kast"))$value," %") })
      
      output$kills_rank <- renderText({ isolate(getPlayerRank("nbKills",input$player,formatted_event_2())) })
      output$deaths_rank <- renderText({ isolate(getPlayerRank("nbDeaths",input$player,formatted_event_2())) })
      output$kd_rank <- renderText({ isolate(getPlayerRank("kdDiff",input$player,formatted_event_2())) })
      output$ratio_rank <- renderText({ isolate(getPlayerRank("ratio_kd",input$player,formatted_event_2())) })
      output$assists_rank <- renderText({ isolate(getPlayerRank("nbAssists",input$player,formatted_event_2())) })
      output$fassists_rank <- renderText({ isolate(getPlayerRank("nbFlashAssists",input$player,formatted_event_2())) })
      output$hs_rank <- renderText({ isolate(getPlayerRank("tauxHs",input$player,formatted_event_2())) })
      output$adr_rank <- renderText({ isolate(getPlayerRank("mean_adr",input$player,formatted_event_2())) })
      output$rating_rank <- renderText({ isolate(getPlayerRank("mean_rating",input$player,formatted_event_2())) })
      output$kast_rank <- renderText({ isolate(getPlayerRank("kast",input$player,formatted_event_2())) })
      
      isolate(paste(input$player, " - ",input$team, " - ",input$event))
    } else {
      paste("Aucun joueur correspondant")
    }
  })
  
  # Permet d'afficher les valeurs de chaque statistique pour tous les joueurs d'une équipe
  output$team_plot <- renderPlotly({
    
    input$submitButton
    
    data <- isolate(reactive_db_stat()) 
    
    if (dim(data)[1]>0) {
      ggplot(isolate(reactive_db_team()),aes(x=value,y=stat_name,color=player_name)) + geom_point() 
    }
  })
  
  # Permet d'afficher le rating moyen par mois du joueur superposé sur un boxplot affichant la répartition des ratings moyen de tous les joueurs 
  # des 30 meilleures équipes mondiales
  output$rating_plot_player <- renderPlotly({
    input$submitButton
    
    data <- isolate(reactive_db_stat()) 
    
    if (dim(data)[1]>0) {
      data 
      isolate(ggplot(isolate(reactive_db_rating_boxplot()), aes(x=mois, y=rating_mean),show.legend = FALSE) + 
                geom_boxplot(outlier.size = 0.01) +  geom_point(data=reactive_db_rating_player(), aes(x=mois, y=rating_mean,fill=player_name),show.legend = FALSE)+
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
    }
  })
}