if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")

if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")


df <- as_tibble(read.csv("data/players.csv"))
names(df)

#### PLUS BESOIN DES RANKS
results <- as_tibble(read.csv("data/results.csv"))

teams_rank <- data.frame(results %>% group_by(match_id, team_1, team_2, rank_1, rank_2) %>% summarise())

pivot <- pivot_longer(teams_rank, !c("match_id"), names_to = c(".value"), names_pattern= "(.)")

df_rank <- merge(df,pivot,by.x=c("match_id","team"),by.y=c("match_id","t"))
names(df_rank)[102] <- "rank_team"

df_rank <- as_tibble(merge(df_rank,pivot,by.x=c("match_id","opponent"),by.y=c("match_id","t")))
names(df_rank)[103] <- "rank_opponent"


### Création de l'histogramme avec l'adr 
df$adr <- as.integer(df$adr)

# Pour un tournoi particulier

majorID <- c(1270,1333,1444,1553,1611,1666,1617,2027,2062,2471,2720,3247,3564,3883,4443)
majorName <- c("DH Winter 2013","Katowice 2014","Cologne 2014","DH Winter 2014",
               "Katowice 2015","Cologne 2015","Cluj-Napoca 2015","Colombus 2016","Cologne 2016",
               "ELEAGUE Major 2017","Krakow 2017","ELEAGUE Major 2018","FACEIT Major 2018",
               "Katowice 2019","Berlin 2019")


### Cluj ne contient que des NA donc on l'exclu
rating_majors <-  df %>%filter (event_id %in% majorID & !is.na(rating))

p_histo_r <- ggplot(rating_majors, aes(x=rating)) + 
  geom_histogram(binwidth=0.02,fill="lightblue") +
  ggtitle('Comptes des ratings sur l\'ensemble des MAJORS')
p_histo_r

p_histo_bis_r <- ggplot(rating_majors, aes(x=event_name, y=rating, fill=event_name)) + geom_boxplot()
p_histo_bis_r


# Représenté ADR ou RATING ? La répartition est similaire



# Faire boxplot par équie pour les stats de chaque joueurs (on peut identifier qui contribue le plus dans un domaine (kills, assis, flash assist ...))





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

df_mean_rating <- df %>% group_by(year(date),month(date),player_id,player_name) %>% summarise(rating_mean = mean(rating,na.rm=TRUE))


# On isole les joueurs par tounois puis par pays pour avoir le nombre de représentant(s) de chaque pays par tournoi
pays_tournois_joueur <- df %>% group_by(event_id,event_name,player_id,player_name,country) %>% summarise() 
pays_tournois <- pays_tournois_joueur%>% group_by(event_name,event_id,country) %>% summarise(nbRep = n())

countries = read.csv("data/countries_codes_and_coordinates.csv")

countries %>% filter (country == "China")
countries[countries == "China"]

countries[countries == "UK"] <- "United Kingdom" 
countries[countries == "USA"] <- "United States" 
countries[countries == "Mainland China"] <- "China" 

pays_tournois = merge(pays_tournois,countries,by="country")

pays_tournois$alpha3[pays_tournois$country == "United States"] <- "USA"


worldcountry = geojson_read("data/50m.geojson", what = "sp")


pays_tournois = pays_tournois[order(pays_tournois$alpha3),]
pays_major = pays_tournois %>% filter (event_id %in% majorID)


  
# Create plotting parameters for map
bins = c(1,3,5,7,9,11,13,15,16)
cv_pal <- colorBin("Oranges", domain = pays_major$nbRep,bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% pays_major$alpha3, ]


cluj = pays_major %>% filter(event_id ==1617)

# creat cv base map 
# Ajouter interraction : liste des joueurs quand on clique dessus
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    options = layersControlOptions(collapsed = FALSE)) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-60,~60,70) %>%
  addLegend("bottomright", pal = cv_pal, values = ~pays_major$nbRep,
            title = "<small>Représentant par pays</small>")
  

basemap



##########

df_reduit <- df_rank %>% filter(rank_team <= 30 & rank_opponent <= 30)

df_mean_adr <- df_reduit %>% group_by(mois = format(as.Date(date), "%Y-%m"),player_id,player_name) %>% summarise(adr_mean = mean(adr,na.rm=TRUE))

df_mean_rating <- df_reduit %>% group_by(mois = format(as.Date(date), "%Y-%m"),player_id,player_name) %>% summarise(rating_mean = mean(rating,na.rm=TRUE))

######
df_stats <- df %>% filter(event_id %in% majorID) %>% group_by(event_id,event_name,player_id,player_name,team) %>% 
  summarise(nbKills = sum(kills,na.rm=TRUE),nbDeaths = sum(deaths,na.rm=TRUE),kdDiff = sum(kddiff,na.rm = TRUE),
            ratio_kd = round((nbKills/nbDeaths )*100,digits=2), tauxHs = round(sum(hs,na.rm=TRUE)/nbKills*100,digits=0),nbAssists =sum(assists,na.rm=TRUE),
            nbFlashAssists = sum(flash_assists,na.rm=TRUE),kast = round(mean(kast, na.rm=TRUE),digits=0),
            mean_adr = round(mean(adr,na.rm=TRUE),digits=2), mean_rating = round(mean(rating,na.rm=TRUE)*100,digits=2))

# on pivote

df_stats_long <- pivot_longer(df_stats, !c("event_id","event_name","player_id","player_name","team"), names_to = "stat_name", values_to = "value")


##########


getPlayerRank <- function (stat,player,event){
  df_order <- df_stats_long %>% filter(event_id == event & stat_name==stat) %>% arrange(desc(value))
  paste(which(df_order$player_name %in% player)[1],"/",length(df_order$player_name))
}

source("ui.R",local = TRUE)
source("server.R",local = TRUE)

shinyApp(ui, server)
