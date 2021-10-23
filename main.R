library(lubridate)
library(tidyr)
library(tidyverse)

df <- as_tibble(read.csv("data/players.csv"))
names(df)

results <- as_tibble(read.csv("data/results.csv"))

median(results$rank_1)

teams_rank <- data.frame(results %>% group_by(match_id, team_1, team_2, rank_1, rank_2) %>% summarise()) 

pivot <- pivot_longer(teams_rank, !c("match_id"), names_to = c(".value"), names_pattern= "(.)")

df <- merge(df,pivot,by.x=c("match_id","team"),by.y=c("match_id","t"))

df <- as_tibble(merge(df,pivot,by.x=c("match_id","opponent"),by.y=c("match_id","t")))

rating_moyen_joueur_année_nbMatch <- df %>% group_by(year(date),player_id,player_name) %>% summarise(nbMatch = n(),mean_rating = mean(rating,na.rm = TRUE)) %>% arrange(desc(nbMatch))
rating_moyen_joueur_année_nbMatch %>% filter(player_name == "ZywOo")
names(rating_moyen_joueur_année_nbMatch)[1]<- "Année"
rating_moyen_joueur_année_nbMatch



p <- ggplot(rating_moyen_joueur_année_nbMatch %>% filter(Année == "2018" && nbMatch > 70), aes(x=nbMatch,y=mean_rating))
p <- p + geom_point()
p

# Nécessaire ID car plusieurs pseudo similaire (ALEX par exemple)
nb_match_joueur_annee <- df %>% group_by(year(date),player_name,player_id) %>% summarise(nbMatch=n()) %>% arrange(desc(nbMatch))
names(nb_match_joueur_annee)[1] <- "Année" 
nb_match_joueur_annee

p <- ggplot(nb_match_joueur_annee %>% filter(player_name == "shox"), aes(x=Année,y=nbMatch))
p <- p + geom_line()
p

# Division par 10 car 10 observations par équipe (5 joueurs par équipe)
nb_match_tournoi <- df %>% group_by(event_name) %>% summarise(nbObservation=n()/10) %>% arrange(desc(nbObservation))
nb_match_tournoi

rating_joueur_tournoi <- df %>% group_by(event_name,event_id,player_name,player_id) %>% summarise(mean_rating = mean(rating)) %>% arrange(desc(mean_rating))
rating_joueur_tournoi %>% filter(event_name == "ESEA MDL Season 30 Europe")

rating_pays <- df %>% group_by(country) %>% summarise(mean_rating = mean(rating)) %>% arrange(desc(mean_rating))
rating_pays


### Création de l'histogramme avec l'adr 
df$adr <- as.integer(df$adr)
adr_tournoi <- df %>% group_by(event_name,event_id,adr)
p <- ggplot(adr_tournoi, aes(x=adr)) + 
  geom_histogram(binwidth=2)
p

# Pour un tournoi particulier
adr_t1 <- adr_tournoi %>% filter (event_id==4443)

p_histo <- ggplot(adr_t1, aes(x=adr)) + 
  geom_histogram(binwidth=2)
p

majorID <- c(1270,1333,1444,1553,1611,1666,1617,2027,2062,2471,2720,3247,3564,3883,4443)
majorName <- c("DH Winter 2013","Katowice 2014","Cologne 2014","DH Winter 2014",
               "Katowice 2015","Cologne 2015","Cluj-Napoca 2015","Colombus 2016","Cologne 2016",
               "ELEAGUE Major 2017","Krakow 2017","ELEAGUE Major 2018","FACEIT Major 2017",
               "Katowice 2019","Berlin 2019")

# Sur l'ensemble des majors
adr_majors <- adr_tournoi %>% filter (event_id %in% majorID)

p_histo <- ggplot(adr_majors, aes(x=adr)) + 
  geom_histogram(binwidth=2) +
  ggtitle('Comptes des adr sur l\'ensemble des MAJORS')
  


 if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")


# On isole les joueurs par tounois puis pay pays pour avopir le nombre de représentant d chaque pays par tournoi
pays_tournois <- df %>% group_by(event_id,event_name,player_id,player_name,country) %>% summarise() %>% group_by(event_name,event_id,country) %>% summarise(nbRep = n())


pays_tournois[pays_tournois == "United Kingdom"] <- "UK" 
pays_tournois[pays_tournois == "United States"] <- "USA" 

countries = read.csv("data/countries_codes_and_coordinates.csv")

names(countries)


pays_tournois =merge(pays_tournois,countries,by="country")
pays_tournois

worldcountry = geojson_read("data/50m.geojson", what = "sp")

pays_tournois = pays_tournois[order(pays_tournois$alpha3),]


# create plotting parameters for map
bins = c(1,3,5,7,9,11,13,15,Inf)
cv_pal <- colorBin("Oranges", domain = pays_tournois$nbRep,bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% pays_tournois$alpha3, ]


# creat cv base map 
# Ajouter interreaction : liste des joueurs quand on clique dessus
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

library(shiny)

ui <- bootstrapPage(
  
  # Application title
  titlePanel("Premier DashBoard"),
  
  div(class="mainDiv",
    tags$head(includeCSS("styles.css")),
    leafletOutput("mymap", width="100%", height="50%"),
    plotOutput("distPlot"),
    absolutePanel(id = "controls", class = "panel panel-default",
                  top = 80, left = 80, width = 300, fixed=TRUE,
                  draggable = TRUE, height = "auto",
                  sliderTextInput("plot_event",
                      label = h5("Choix du tournoi"),
                      choices = majorName[8:length(majorName)],
                      selected = majorName[length(majorName)],
                      grid = FALSE,
                      animate=animationOptions(interval = 3000, loop = FALSE))
    ),
  )
 
)

server <- function(input, output, session) {
  
  output$distPlot <- renderPlot({
    p_histo
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
  
  observeEvent(input$plot_event, {
    leafletProxy("mymap") %>% 
      clearShapes() %>%
      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 1, fillOpacity = 0.5, fillColor = ~cv_pal(reactive_db()$nbRep))
    
  })
}

shinyApp(ui, server)
