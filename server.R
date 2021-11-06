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
  
  output$firstHistoPlot <- renderPlotly({
    p_histo_r
  })
  output$secondHistoPlot <- renderPlotly({
    p_histo_bis_r
  })
  
  output$kd_plot <- renderPlotly({
    p_kd
  })
  
  
  formatted_event = reactive({
    majorID[match(input$plot_event,majorName)]
  })
  
  output$mymap <- renderLeaflet({
    basemap
  })
  
  reactive_polygons = reactive({
    worldcountry[worldcountry$ADM0_A3 %in% reactive_db()$alpha3, ]
  })
  
  reactive_db = reactive({
    pays_major %>% filter(event_id == formatted_event())
  })
  
  reactive_label = reactive({
    sprintf(
      "<strong>%s</strong><br/>%g",
      reactive_db()$country, reactive_db()$nbRep
    ) %>% lapply(htmltools::HTML)
    
  })
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
  
  
  ### Joueur
  
  formatted_event_2 = reactive({
    majorID[match(input$event,majorName)]
  })
  
  reactive_db_team = reactive({
    df_stats_long %>% filter(team == input$team & event_id == formatted_event_2() & stat_name %in% c("ratio_kd","nbAssists","kast","mean_rating","mean_adr","nbFlashAssists","tauxHs"))
  })
  
  
  reactive_mois_rating = reactive({
    unique((df_mean_rating %>% filter(player_name == input$player & !is.na(rating_mean)))$mois)
  })
  
  reactive_db_rating_boxplot = reactive({
    df_mean_rating %>% filter (mois %in% reactive_mois_rating() )
  })
  
  reactive_db_rating_player = reactive({
    df_mean_rating %>% filter(player_name %in% c(input$player) & mois %in% reactive_mois_rating())
  })
  
  reactive_db_stat  = reactive({
    df_stats_long %>% filter(player_name == input$player & event_id == formatted_event_2() & team == input$team)
  })
  
  output$value <- renderText({ 
    input$submitButton 
    
    stats <- isolate(reactive_db_stat()) 
    
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
  
  
  output$team_plot <- renderPlotly({
    input$submitButton
    
    
    data <- isolate(reactive_db_stat()) 
    if (dim(data)[1]>0) {
      ggplot(isolate(reactive_db_team()),aes(x=value,y=stat_name,color=player_name)) + geom_point() 
    }
  })
  
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