library(lubridate)
library(tidyr)
library(tidyverse)

df <- as_tibble(read.csv("data/players.csv"))
names(df)

#### PLUS BESOIN DES RANKS
results <- as_tibble(read.csv("data/results.csv"))

teams_rank <- data.frame(results %>% group_by(match_id, team_1, team_2, rank_1, rank_2) %>% summarise())

pivot <- pivot_longer(teams_rank, !c("match_id"), names_to = c(".value"), names_pattern= "(.)")
names(df)

df <- merge(df,pivot,by.x=c("match_id","team"),by.y=c("match_id","t"))
names(df)[102] <- "rank_team"

df <- as_tibble(merge(df,pivot,by.x=c("match_id","opponent"),by.y=c("match_id","t")))
names(df)[103] <- "rank_opponent"
names(df)


### Création de l'histogramme avec l'adr 
df$adr <- as.integer(df$adr)

# Pour un tournoi particulier

majorID <- c(1270,1333,1444,1553,1611,1666,1617,2027,2062,2471,2720,3247,3564,3883,4443)
majorName <- c("DH Winter 2013","Katowice 2014","Cologne 2014","DH Winter 2014",
               "Katowice 2015","Cologne 2015","Cluj-Napoca 2015","Colombus 2016","Cologne 2016",
               "ELEAGUE Major 2017","Krakow 2017","ELEAGUE Major 2018","FACEIT Major 2017",
               "Katowice 2019","Berlin 2019")

# Sur l'ensemble des majors


### Cluj ne contient que des NA donc on l'exclu
adr_majors <-  df %>%filter (event_id %in% majorID & !is.na(adr)) %>% group_by(event_name,event_id,adr)


p_histo <- ggplot(adr_majors, aes(x=adr)) + 
  geom_histogram(binwidth=2,fill="lightblue") +
  ggtitle('Comptes des adr sur l\'ensemble des MAJORS')
p_histo 


p_histo_bis <- ggplot(adr_majors, aes(x=event_name, y=adr, fill=event_name)) + geom_boxplot()
p_histo_bis

rating_majors <-  df %>%filter (event_id %in% majorID & !is.na(rating))


p_histo_r <- ggplot(rating_majors, aes(x=rating)) + 
  geom_histogram(binwidth=0.02,fill="lightblue") +
  ggtitle('Comptes des ratings sur l\'ensemble des MAJORS')
p_histo_r

p_histo_bis_r <- ggplot(rating_majors, aes(x=event_name, y=rating, fill=event_name)) + geom_boxplot()
p_histo_bis_r


# Représenté ADR ou RATING ? La répartition est similaire



# Faire boxplot par équie pour les stats de chaque joueurs (on peut identifier qui contribue le plus dans un domaine (kills, assis, flash assist ...))



if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")


# On isole les joueurs par tounois puis par pays pour avoir le nombre de représentant(s) de chaque pays par tournoi
pays_tournois <- df %>% group_by(event_id,event_name,player_id,player_name,country) %>% summarise() %>% group_by(event_name,event_id,country) %>% summarise(nbRep = n())


pays_tournois[pays_tournois == "United Kingdom"] <- "UK" 
pays_tournois[pays_tournois == "United States"] <- "USA" 

countries = read.csv("data/countries_codes_and_coordinates.csv")

pays_tournois =merge(pays_tournois,countries,by="country")



### KD PAR TOURNOIS
df_stats <- df %>% filter(event_id %in% majorID) %>% group_by(event_id,event_name,player_id,player_name,team) %>% 
  summarise(nbKills = sum(kills,na.rm=TRUE),nbDeaths = sum(deaths,na.rm=TRUE),kdDiff = sum(kddiff,na.rm = TRUE),ratio_kd = nbKills/nbDeaths, tauxHs = sum(hs,na.rm=TRUE),nbAssists =sum(assists,na.rm=TRUE),nbFlashAssists = sum(flash_assists,na.rm=TRUE),kast = mean(kast, na.rm=TRUE),mean_adr = mean(adr,na.rm=TRUE), mean_rating = mean(rating,na.rm=TRUE))
df_stats_long <- pivot_longer(df_stats, !c("event_id","event_name","player_id","player_name","team"), names_to = "stat_name", values_to = "value")

df_stats_majors <- df_stats %>% filter(event_id %in% majorID)

p_kd <- ggplot(df_stats_majors, aes(x=nbDeaths,y=nbKills))
p_kd <- p_kd + geom_point(size=0.75,aes(color=team))  + geom_abline(intercept = 0, slope = 1,linetype="dotted",size=0.5,color="lightgrey")
p_kd <- p_kd+ facet_wrap(~ event_name, nrow = 4)
p_kd

###  Stats des joueurs

df_mean_adr <- df %>% group_by(mois = format(as.Date(date), "%Y-%m"),player_id,player_name) %>% summarise(adr_mean = mean(adr,na.rm=TRUE))
df_mean_adr %>% filter(player_name == "ZywOo")

df_mean_rating <- df %>% group_by(year(date),month(date),player_id,player_name) %>% summarise(rating_mean = mean(rating,na.rm=TRUE))
df_mean_rating %>% filter(player_name == "f0rest")



worldcountry = geojson_read("data/50m.geojson", what = "sp")

pays_tournois = pays_tournois[order(pays_tournois$alpha3),]
pays_major = pays_tournois %>% filter (event_id %in% majorID)

  
# Create plotting parameters for map
bins = c(1,3,5,7,9,11,13,15,16)
cv_pal <- colorBin("Oranges", domain = pays_major$nbRep,bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% pays_tournois$alpha3, ]


# creat cv base map 
# Ajouter interraction : liste des joueurs quand on clique dessus
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    options = layersControlOptions(collapsed = FALSE)) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-60,~60,70) %>%
  addLegend("bottomright", pal = cv_pal, values = ~pays_tournois$nbRep,
            title = "<small>Représentant par pays</small>")

if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")

library(shiny)
library(plotly)


##########

df_reduit <- df %>% filter(rank_team <= 30 & rank_opponent <= 30)

df_mean_adr <- df_reduit %>% group_by(mois = format(as.Date(date), "%Y-%m"),player_id,player_name) %>% summarise(adr_mean = mean(adr,na.rm=TRUE))

df_mean_rating <- df_reduit %>% group_by(mois = format(as.Date(date), "%Y-%m"),player_id,player_name) %>% summarise(rating_mean = mean(rating,na.rm=TRUE))

######
df_stats <- df %>% filter(event_id %in% majorID) %>% group_by(event_id,event_name,player_id,player_name,team) %>% 
  summarise(nbKills = sum(kills,na.rm=TRUE),nbDeaths = sum(deaths,na.rm=TRUE),kdDiff = sum(kddiff,na.rm = TRUE),
            ratio_kd = round(nbKills/nbDeaths*100,digits=2), tauxHs = round(sum(hs,na.rm=TRUE)/nbKills*100,digits=0),nbAssists =sum(assists,na.rm=TRUE),
            nbFlashAssists = sum(flash_assists,na.rm=TRUE),kast = round(mean(kast, na.rm=TRUE),digits=0),
            mean_adr = round(mean(adr,na.rm=TRUE),digits=2), mean_rating = round(mean(rating,na.rm=TRUE)*100,digits=2))

# on pivote

df_stats_long <- pivot_longer(df_stats, !c("event_id","event_name","player_id","player_name","team"), names_to = "stat_name", values_to = "value")


##########
ui <- navbarPage("Navbar!", 
         tags$head(includeCSS("styles.css")),
         tabPanel("Carte",
              div(class="mainContainer",
                
                leafletOutput("mymap", width="100%", height="100%"),
                absolutePanel(id = "controls", class = "panel panel-default",
                            top = 80, left = 80, width = 300, fixed=TRUE,
                            draggable = TRUE, height = "auto",
                            sliderTextInput("plot_event",
                                label = h5("Choix du tournoi"),
                                choices = majorName[8:length(majorName)],
                                selected = majorName[length(majorName)],
                                grid = FALSE,
                                animate=animationOptions(interval = 3000, loop = FALSE))
                )
              )
              
         ),
         tabPanel("Histogramme",
            div(class="mainContainer",
              plotlyOutput("firstHistoPlot", width="100%", height="50%"),
              plotlyOutput("secondHistoPlot", width="100%", height="50%")
            )
         ),
         tabPanel("KD",
            div(class="mainContainer",
                plotlyOutput("kd_plot", width="100%", height="100%")   
            )
         ),
         tabPanel("Joueur",
            div(class="mainContainer",
                div(class="recherche",
                    tags$head(includeCSS("styles.css")),
                    textInput("player",NULL,placeholder = "Player"),
                    textInput("team",NULL,placeholder ="Team"),
                    selectInput(
                      "event",
                      NULL,
                      majorName[8:length(majorName)]),
                    actionButton("submitButton", "Valider")
                ),
                verbatimTextOutput("value"),
                div(id="stats",
                    textOutput("kills_value"),
                    textOutput("deaths_value"),
                    textOutput("kd_value"),
                    textOutput("ratio_value"),
                    textOutput("hs_value"),
                    textOutput("assists_value"),
                    textOutput("fassists_value"),
                    textOutput("adr_value"),
                    textOutput("rating_value"),
                    textOutput("kast_value")
                  ),
                div(class="boxplots_container",
                    plotlyOutput("team_plot", width="50%", height="100%"),
                    plotlyOutput("rating_plot_player", width="50%", height="100%")
                ),
            )
         )
)

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
    pays_tournois %>% filter(event_id == formatted_event())
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
      addPolygons(data = reactive_polygons(),
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
      output$ratio_value <- renderText({ paste("Ratio : ",(stats %>% filter (stat_name == "ratio_kd"))$value /100)})
      output$assists_value <- renderText({ paste("Nombre d'Assists :",(stats %>% filter (stat_name == "nbAssists"))$value)})
      output$hs_value <- renderText({ paste("Taux de Headshot :",(stats %>% filter (stat_name == "tauxHs"))$value," %") })
      output$fassists_value <- renderText({ paste("Nombre de Flash Assists :",(stats %>% filter (stat_name == "nbFlashAssists"))$value)})
      output$adr_value <- renderText({ paste("ADR Moyen:",(stats %>% filter (stat_name == "mean_adr"))$value)})
      output$rating_value <- renderText({ paste("Rating Moyen : ",(stats %>% filter (stat_name == "mean_rating"))$value /100)})
      output$kast_value <- renderText({ paste("KAST :",(stats %>% filter (stat_name == "kast"))$value," %") })
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

shinyApp(ui, server)
