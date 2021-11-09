if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")

if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tibble)) install.packages("tibble", repos = "http://cran.us.r-project.org")


# Commençons par importer les données

df <- as_tibble(read.csv("data/players.csv"))


# Importation des données nécessaires pour ajouter le classement des équipes
results <- as_tibble(read.csv("data/results.csv"))



# On isole pour chaque match le classement des deux équipes
teams_rank <- data.frame(results %>% group_by(match_id, team_1, team_2, rank_1, rank_2) %>% summarise())

# On pivote le df précédent pour enlever la notion équipe 1 / équipe 2
# On obtient ainsi un df avec : match_id - équipe - classement
pivot <- pivot_longer(teams_rank, !c("match_id"), names_to = c(".value"), names_pattern= "(.)")

# Nous pouvons commencer la fusion en ajoutant le classement aux df avec les statistiques des joueurs
# La fusion va avoir lieu sur l'id du match et le nom de la team du joueur en matchant l'id et nom d'équipe dans le secon dataframe
df <- merge(df,pivot,by.x=c("match_id","team"),by.y=c("match_id","t"), all.x=TRUE)
names(df)[102] <- "rank_team"

# On réitère l'opértion précédente mais on rajoute le classement de l'équipe adverse
df <- as_tibble(merge(df,pivot,by.x=c("match_id","opponent"),by.y=c("match_id","t"), all.x=TRUE))
names(df)[103] <- "rank_opponent"

# On crée deux vecteurs qui permettent d'isoler les majors
# Nous sommes obligés d'écrire les valeurs en dur car il n'y a pas moyen d'identifier un major uniquement
# par son nom. On pourrait penser à chercher ceux dont le nom contient Major mais le df retournera aussi
# les tournois qualificatif qui contiennent major dans le nom.
majorID <- c(1270,1333,1444,1553,1611,1666,1617,2027,2062,2471,2720,3247,3564,3883,4443)
majorName <- c("DH Winter 2013","Katowice 2014","Cologne 2014","DH Winter 2014",
               "Katowice 2015","Cologne 2015","Cluj-Napoca 2015","Colombus 2016","Cologne 2016",
               "ELEAGUE Major 2017","Krakow 2017","ELEAGUE Major 2018","FACEIT Major 2018",
               "Katowice 2019","Berlin 2019")


### CREATION DE L'HISTOGRAMME

# On commencer par isoler toutes les observations ayant lieu dans un major et où la valeur du rating est disponible (not NA)
rating_majors <-  df %>%filter (event_id %in% majorID & !is.na(rating))

## On peut ensuite tracer l'histogramme et le boxplot

p_histo_r <- ggplot(rating_majors, aes(x=rating)) + 
  geom_histogram(binwidth=0.02,fill="lightblue") +
  ggtitle('Comptes des ratings sur l\'ensemble des MAJORS')

p_histo_bis_r <- ggplot(rating_majors, aes(x=event_name, y=rating, fill=event_name)) + geom_boxplot()

### Calcul de l'ensemble des statistiques par joueur sur un tournoi

# Acttuellement, une observation correspond à un match d'un tournoi
# On va regrouper les données pour qu'une observation correspondent à un tournoi


######
df_stats <- df %>% filter(event_id %in% majorID) %>% group_by(event_id,event_name,player_id,player_name,team) %>% 
  summarise(nbKills = sum(kills,na.rm=TRUE),nbDeaths = sum(deaths,na.rm=TRUE),kdDiff = sum(kddiff,na.rm = TRUE),
            ratio_kd = round((nbKills/nbDeaths )*100,digits=2), tauxHs = round(sum(hs,na.rm=TRUE)/nbKills*100,digits=0),
            nbAssists =sum(assists,na.rm=TRUE), nbFlashAssists = sum(flash_assists,na.rm=TRUE),
            kast = round(mean(kast, na.rm=TRUE),digits=0), mean_adr = round(mean(adr,na.rm=TRUE),digits=2), 
            mean_rating = round(mean(rating,na.rm=TRUE)*100,digits=2))

# on pivote 

df_stats_long <- pivot_longer(df_stats, !c("event_id","event_name","player_id","player_name","team"), names_to = "stat_name", values_to = "value")

########## On calcule maintenant le rating moyen par mois d'un joueur. 
#On ne sélectionne que ceux lors d'un match entre équipe du top 30 mondial ou bien lors d'un match d'un tournoi Major

df_reduit <- df %>% filter((rank_team <= 30 & rank_opponent <= 30) | event_id %in% majorID)

df_mean_rating <- df_reduit %>% group_by(mois = format(as.Date(date), "%Y-%m"),player_id,player_name) %>% 
                  summarise(rating_mean = mean(rating,na.rm=TRUE))

##########

df_stats_majors <- df_stats %>% filter(event_id %in% majorID)

p_kd <- ggplot(df_stats_majors, aes(x=nbDeaths,y=nbKills))
p_kd <- p_kd + geom_point(size=0.75,aes(color=team))  + geom_abline(intercept = 0, slope = 1,linetype="dotted",size=0.5,color="lightgrey")
p_kd <- p_kd+ facet_wrap(~ event_name, nrow = 4)


######## CREATION DE LA CARTE

# On isole les joueurs par tounois puis par pays pour avoir le nombre de représentant(s) de chaque pays par tournois
pays_tournois_joueur <- df %>% group_by(event_id,event_name,player_id,player_name,country) %>% summarise() 
pays_tournois <- pays_tournois_joueur%>% group_by(event_name,event_id,country) %>% summarise(nbRep = n())


countries = read.csv("data/countries_codes_and_coordinates.csv")

# On rename afin d'uniformiser la nomenclature des pays et de pouvoir merge
countries[countries == "UK"] <- "United Kingdom" 
countries[countries == "USA"] <- "United States" 
countries[countries == "Mainland China"] <- "China" 

pays_tournois = merge(pays_tournois,countries,by="country")

# On rename afin d'uniformiser la nomenclature des pays et de pouvoir merge
pays_tournois$alpha3[pays_tournois$country == "United States"] <- "USA"


worldcountry = geojson_read("data/50m.geojson", what = "sp")


pays_tournois = pays_tournois[order(pays_tournois$alpha3),]
pays_major = pays_tournois %>% filter (event_id %in% majorID)


  
# Create plotting parameters for map
bins = seq(1,max(pays_major$nbRep),by=2)
bins[length(bins)] <- Inf
cv_pal <- colorBin("Oranges", domain = pays_major$nbRep,bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% pays_major$alpha3, ]



# Création du fond de map
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    options = layersControlOptions(collapsed = FALSE)) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-60,~60,70) %>%
  addLegend("bottomright", pal = cv_pal, values = ~pays_major$nbRep,
            title = "<small>Représentant par pays</small>")
  
# Cette fonction permet de retourner le classement du joueur pour une statistique sur un tournoi
# On l'utilise si on souhaite connaitre le classment de "s1mple" sur le tournoi ayant pour ID 4443
# sur la statistique "nbKills"
getPlayerRank <- function (stat,player,event){
  df_order <- df_stats_long %>% filter(event_id == event & stat_name==stat) %>% arrange(desc(value))
  paste(which(df_order$player_name %in% player)[1],"/",length(df_order$player_name))
}

source("ui.R",local = TRUE)
source("server.R",local = TRUE)

shinyApp(ui, server)
