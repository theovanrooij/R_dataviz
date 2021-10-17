library(lubridate)
library(tidyr)
library(tidyverse)

df <- as_tibble(read.csv("data/players.csv"))
names(df)

results <- as_tibble(read.csv("data/results.csv"))

median(results$rank_1)

# Pas tous les ids de chaque table. Dans result, on retrouve des tournois peu populaires et pas dans players
diff <-setdiff(id_df,id_result)
length(diff)

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

# KD


# Nécessaire ID car plusieurs pseudo sont similaires (ALEX par exemple)
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


if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")


# On isole les joueurs par tounois puis par pays pour avoir le nombre de représentant(s) de chaque pays par tournoi
pays_tournois <- df %>% group_by(event_id,event_name,player_id,player_name,country) %>% summarise() %>% group_by(event_name,event_id,country) %>% summarise(nbRep = n())


starlader = pays_tournois %>% filter(event_id == 4443)

starlader[starlader == "United Kingdom"] <- "UK" 
starlader[starlader == "United States"] <- "USA" 

countries = read.csv("data/countries_codes_and_coordinates.csv")

names(countries)


starlader =merge(starlader,countries,by="country")

worldcountry = geojson_read("data/50m.geojson", what = "sp")

starlader = starlader[order(starlader$alpha3),]


# Create plotting parameters for map
bins = c(1,3,5,7,9,11,13,15,Inf)
cv_pal <- colorBin("Oranges", domain = starlader$nbRep,bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% starlader$alpha3, ]

# Create cv base map 
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    options = layersControlOptions(collapsed = FALSE)) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-60,~60,70) %>%
  addLegend("bottomright", pal = cv_pal, values = ~starlader$nbRep,
            title = "<small>Représentant par pays</small>") %>%
  addPolygons(stroke = FALSE, smoothFactor = 1, fillOpacity = 0.5, fillColor = ~cv_pal(starlader$nbRep))

basemap

