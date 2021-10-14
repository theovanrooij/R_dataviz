library(lubridate)
library(tidyr)
library(tidyverse)

df <- as_tibble(read.csv("players.csv"))
names(df)

results <- as_tibble(read.csv("results.csv"))

median(results$rank_1)

# Pas tous les ids de chaque table. Dans result, on retrouve des tournois peu populaire et pas dans players
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

# On isole les joueurs par tounois puis pay pays pour avopir le nombre de représentant d chaque pays par tournoi
pays_tournois <- df %>% group_by(event_id,event_name,player_id,player_name,country) %>% summarise() %>% group_by(event_name,country) %>% summarise(nbRep = n())
pays_tournois

MAJORS <- c("StarLadder Berlin Major 2019","IEM Katowice Major 2019","FACEIT Major: London 2018","ELEAGUE Major: Boston 2018","PGL Major: Kraków 2017")
MAJORS
unique((df %>% filter(event_name =="PGL Major: Kraków 2017"))$event_name)

which(unique(df$event_name) %in% MAJORS)

df %>% filter(event_name %in% MAJORS)
