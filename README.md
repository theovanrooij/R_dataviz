VAN ROOIJ Théo et DANG Méline
# Introduction

  Nous avons choisi d'étudier un dataset répertoriant les données générés par les joueurs lors de matchs compétitifs du jeu Counter Strike : Global Offensive entre Octobre 2015 et Février 2020.

Le jeu de données n'est pas présent dans le répertoire car il est trop lourd ( > 100 MB) et est à retrouver à l'adresse suivante : https://www.kaggle.com/mateusdmachado/csgo-professional-matches

Afin d'étudier le dataset, il nous faut comprendre le but du jeu. 
CS:GO est un jeu de tir à le première personne où le but est de faire exploser une bombe pour les attaquants et de défendre les sites ou de défuser la bombe pour les défenseurs.  Pour cela les équipes s'affrontent lors de rounds de 2 minutes. La première équipe à en gagner 16 gagne le match.
CS:GO se joue à 5 contre 5, et les équipes peuvent être de nationalités mixtes. 

Afin d'attaquer ou de défendre efficacement, les joueurs sont amenés à s'entretuer et sont équipés d'utilitaires tels que des grenades flash, utilisées pour aveugler l'adversaire.
Les données mesurées et présentes dans le dataset sont les suivantes :
1. Nombre de kills réalisés par le joueur
2. Nombre de morts du joueur
3. Différence entre les deux 
4. Nombre de headshots (tirs dans la tête)
5. Nombre d'assistances (réalisées lorsque le joueur fait des dégâts à un adversaire mais ne le tue pas)
6. Nombre de flash assistances (le joueur aveugle un adversaire avant qu'un coéquipier ne le tue)
7. KAST : Pourcentage de round dans lequel le joueur a un kill, une assist, a survécu ou " *traded* " (mort amenant à la mort immédiate d'un adversaire)
8. ADR : Nombre moyen de dégâts infligés par le joueur par round (un joueur possède 100 points de vie)
9. Rating : score calculé en comparant  : le nombre de kills par round, le nombre de rounds où le joueur a survécu, et le nombre de round avec au minimum 2 kills effectués par le joueur dans le match avec les valeurs moyennes des même statistiques sur l'ensemble des joueurs. 

Le Rating est un bon indicateur du niveau de performance d'un joueur car il regroupe d'autres statistiques et est comparé avec les valeurs des autres joueurs.

Notre jeu de donnés est composé de 4 fichiers CSV. Nous allons nous concentrer seulement sur le fichier "*players.csv*" qui contient les statistiques des joueurs.

Chaque observation de ce fichier correspond aux statistiques d'un joueur pour un match. Par conséquent, chaque match possède 10 observations puisque 10 joueurs s'affrontent. Sur chaque observation, le tournoi d'origine est renseigné ce qui va nous permettre la réalisation de ce projet.

Nous utilisons également partiellement le fichier "*results.csv*" afin de récupérer le classement des deux équipes au moment du match observé.

Dans ce projet nous allons nous intéresser aux statistiques des joueurs lors des tournois du Majors En nous questionnant sur les caractéristiques d'un joueur professionnel.
 
 Les Majors sont les tournois les plus importants de CS:GO. Ils sont bi-annuels et les équipes s'affrontent durant les six mois au sein de leur continent pour engranger des points. Les équipes avec le plus de points se qualifient ainsi pour le Major.

  Vous pouvez retrouver la liste des majors, des équipes et des joueurs à l'adresse suivante : 
  https://www.hltv.org/events/archive?eventType=MAJOR

  
# User Guide
 ### Package Nécessaire
 Afin de réaliser notre dashboard les packages suivants sont nécessaires :
 
 - lubridate
 - plotly
 - ggplot2
 - shiny
 - shinyWidgets
 - shinydashboard
 - geojsonio
 - leaflet

Si vous ne possédez pas ces packages, le programme s'occupe de les télécharger au début du code.

Le jeu de données étant de taille conséquente ( > 165 MB ). Nous avons créé un fichier download.py qui permet de les télécharger. Vous pouvez ainsi lancer le script avant de lancer l'application, ce qui vous permettra de gagner du temps et de ne pas avoir à les re télécharger à chaque lancement de l'application.

Pour faciliter les tests et éviter la configuration du script python avec l'API Kaggle, nous avons téléchargé le jeux de données et l'avons hébergé sur notre site https://perso.esiee.fr/~vanrooit/R.

Le script nécessite le module suivant : 

 - requests

Pour télécharger les données, suivez les instructions suivantes :

1. Ouvrez votre terminal python favori (Anaconda prompt de préférence car il contient déjà le module "*requests*")
2. Rendez vous dans le dossier contenant notre projet
3. Exécutez la commande  : `python download.py`. L'éxécution peut prendre un certains temps en fonction de votre connection

Vous pouvez maintenant lancez l'application R shiny sans difficultés.
Pour cela, ouvrez le fichier .Rproj dans Rstudio, ouvrez ensuite le fichier `main.R` puis cliquez sur `Run app` en haut de l'onglet.

Si vous n'avez pas les modules nécessaires d'installés, l'application les installe automatiquement, ce qui peut nécessiter un temps d'attente.
 


# Developper Guide
  

Nous avons décomposé notre application en 3 fichiers :
1. main.R contient le traitement des données. Il crée tous les dataframes nécessaires à l'affichage du dashboard
2. ui.R contient le code pour la partie graphique du dashboard
3. server.R contient le code permettant d'afficher les graphiques de l'ui en faisant correspondre les valeurs renseignées par l'utilisateur dans l'ui et les dataframes créés dans la partie main.R

Dans main.R, nous commençons par importer les deux fichiers csv et nous les fusionnons afin d'ajouter le classement des deux équipes au début du match. Ce classement est présent dans *results.csv* et pas dans *players.csv* qui est le fichier dont nous allons nous servir. Le classement nous servira à filtrer les matchs pour ne garder que ceux entre les équipes du top 30 mondial car, à valeur égale, les statistiques des joueurs du top 30 n'ont pas la même importance que celles des joueurs des équipes entre les 150e et 250e places.

Une fois la fusion réalisée, nous réalisons plusieurs opérations d'aggrégation pour créer les datasets nécessaires au bon fonctionnement de l'application.

Pour modifier l'aspect visuel du dashboard, vous pouvez ajouter les éléments dans ui.R et leur appliquer des éléments de style dans `style.css`.

Pour ajouter de nouveaux graphiques la procédure est donc la suivante : 
1. Créer le Dataframe contenant les statistiques à étudier dans `main.R`
2. Ajouter les composants visuels dans `ui.R`
3. Dans `server.R`, ajouter les lignes de code permettant d'afficher les données voulu dans le nouveau composant visuel.
4. Dans `server.R`, ajouter éventuellement les expressions réactives permettant de prendre en compte les inputs remplies par l'utilisateur 
5. Enfin, modifier le fichier `style.css` pour styliser le nouveau graphique

# Rapport d'analyse


## Analyse de la carte
 

### Objectif

La carte que nous avons choisi de créer permet de visualiser la répartition des nationalités des joueurs pour chaque tournoi des Majors.


### Construction

Afin de construire notre carte, nous nous avons pris exemple sur l'application suivante : 
https://shiny.rstudio.com/gallery/covid19-tracker.html

Nous l'avons notamment utilisée pour l'affichage des données grâce aux fichier GEOJSON présent dans le répertoire GitHub.

Concernant nos données, nous avons créé le dataframe associé en isolant tout d'abord chaque joueur participant au tournoi associé. Nous avons ensuite regroupé tous les joueurs et compté le nombre d’occurrences par pays. Finalement, comme nous nous intéressons aux tournois Majors, nous avons filtré le dataframe pour ne garder que les données associées à ces tournois.  


### Observation

  

Nous avons des valeurs pour 8 tournois.

La première chose que nous pouvons observer est que la seule région non présente est l'Afrique. Cela n'est pas surprenant, l'eSport est peu développé sur le continent africain et les seuls serveurs permettant le jeu en ligne se trouvent en Afrique du Sud.

  
Nous pouvons ensuite identifier les pays majeurs qui envoient, à chaque tournoi, un nombre conséquent de représentants. Ces pays sont : Les États-Unis, la France, le Danemark, la Suède ainsi que la Russie et l'Ukraine qui, dans les faits, ont souvent des équipes mixant les deux pays. En plus de ces pays majeurs, nous retrouvons d'autres nations qui sont constamment représentées par quelques joueurs : le Brésil avec minimum une équipe par tournoi, mais aussi le Kazakhstan, la Finlande, le Canada ou encore l'Allemagne.

  
Concernant l'Asie, le continent n'a réussi qu'à qualifier qu'une seule équipe pour un Major composé de 3 joueurs Chinois et deux Indonésiens.

  
Enfin, en Océanie, seule l'Australie à envoyer des représentants et cela s'est produit lors des deux derniers tournois.



## Analyse de l'histogramme

### Objectif

Nous souhaitons maintenant étudier la répartition des rating. Vous pouvez retrouver la définition de cette mesure dans l'introduction.
Les valeurs de rating varient en majorité entre 0 et 2 par pas de 0.01.

### Construction

Les ratings sont des valeurs présentes dans le dataset. Nous n'avons pas besoin de réaliser d'opérations particulières pour les isoler. Nous traçons ensuite deux graphiques.
1. Un histogramme comptant et affichant la répartition des ratings pour chaque joueur à chaque match sur l'ensemble des Majors
2. Un Boxplot qui permet de voir le détail de cette répartition pour chaque Major

### Analyse

En analysant l'histogramme, nous remarquons que la répartition est proche d'une fonction gaussienne de moyenne/médiane égale à 1. 
Nous nous resservirons de cet histogramme par la suite, dans l'analyse des statistiques de joueurs, pour donner de la valeur au rating moyen que nous calculerons.

Sur le graphique boxplot, nous affichons plus de détail sur cette répartition en isolant chaque tournoi. 
Nous remarquons qu'elle est indépendante du tournoi. En effet, tous les tournois des Majors ont des valeurs relativement similaires : une médiane entre 1.02 et 1.05, à l'exception de la *DreamHack Open Cluj-Napoca 2015* qui vaut 0.97, un premier quantile entre 0.79 et 0.87 (la majorité proche de 0.85) et un troisième quantile entre 1.18 et 1.27. 



## Analyse de la différence Kill-Deaths

### Objectif

L'objectif de ce graphique est d'afficher, pour chaque tournoi, le nombre de kills et de morts pour chaque joueur.

### Construction
Pour construire le dataframe associé, nous avons juste groupé les données par joueur et par tournoi, puis nous avons additionné le nombre de kills et morts. Nous avons également calculé le ratio en divisant le nombre de kills par celui de morts. 
Le graphique associé est un nuage de points ayant pour abscisse le nombre de morts et en ordonné le nombre de kills. Nous avons également tracé la droite d'équation y = x. Ainsi tous les points (joueurs) situés au dessus de cette droite ont un ratio positif (plus de kills que de morts).

### Analyse


Pour chaque tournoi, nous pouvons distinguer trois zones principales :
1. Une zone où le nombre de kills et le nombre de morts est faible. 
2. Une zone où le nombre de kills et le nombre de morts est dans la moyenne. 
3. Une zone où le nombre de kills et le nombre de morts est élevé. 

Pour chaque zone, nous remarquons que le ratio est différent. En effet, dans la zone 1, la majorité des joueurs ont un ratio négatif alors que dans la troisième, la majorité des joueurs ont un ratio positif. Dans la seconde zone, il y a autant de joueurs au ratio négatif qu'au ratio positif.
Ces observations ne sont pas surprenantes car nous pouvons également à partir de ce graphique, identifier quelles sont les équipes ayant été le plus loin dans la compétition. Plus une équipe va loin dans la compétition, plus elle joue de matchs et par conséquent, plus le nombre de kills et de morts est important. De plus, plus une équipe va loin, plus ses joueurs ont un niveau élevé. Nous nous attendons donc à ce que leur ratio soit plus élevé, ce qui est le cas pour la majorité des joueurs. 


## Analyse des statistiques d'un joueur

### Objectif

Dans cette partie, nous allons nous intéresser aux statistiques d'un joueur et de son équipe lors d'un tournoi des Majors. 

### Construction
Notre jeu de données contient un certain nombre de mesures précisées dans l'introduction. Notre travail dans cette partie a donc été de regrouper ces mesures puis de les sommer pour le nombre de kills, morts, assists et flash assists. Nous avons ensuite pu calculer la différence kill-deaths ainsi que le ratio. Nous avons également calculer la moyenne pour l'ADR, le rating et le KAST. Enfin, le taux de headshots est calculé en divisant le nombre total de headshots par le nombre total de kills. 

Finalement, nous avons choisi d'afficher ces données telles quelles dans un premier temps. Nous avons ensuite créé deux graphiques. Le premier permet de visualiser l'ensemble de ces statistiques pour l'équipe du joueur et donc de déterminer où se situe le joueur au sein de son équipe. 
Afin de faciliter la lisibilité du graphique, nous avons multiplié les valeurs de ratio et de rating par 100 car elles varient entre 0 et 2 alors que les autres variables peuvent varier entre 0 et 100.

Le second n'est pas lié aux Majors. Il met en valeur l'évolution du rating moyen du joueur pour chaque mois depuis le début du dataset ou bien le début de sa carrière si elle a commencé après les premières observations. Nous avons superposé cette moyenne sur un boxplot calculé sur l'ensemble des rating des joueurs lors d'un match entre les 30 meilleures équipes mondiales. Nous pouvons ainsi facilement visualiser où se situe le joueur par rapport aux autres.

### Analyse

Dans cette analyse, nous allons concentrer notre analyse sur le joueur ***s1mple*** de l'équipe ***Natus Vincere*** lors du tournoi organisé à Katowice en 2019. Il est considéré comme un des deux meilleurs joueurs au monde.

Comme voulu, nous avons accès à toutes les statistiques sur le haut de la page. Nous pouvons également comparer le rating moyen obtenu avec les boxplot obtenus sur l'onglet histogramme. Le rating moyen de s1mple sur ce tournoi est de 1.31. En regardant le boxplot correspondant, il se situe bien au dessus du troisième quartile qui est de 1.22. Le rating étant une métrique permettant de quantifier les performances d'un joueur, on en déduit qu'il a fait partie des meilleurs joueurs du tournoi. En regardant le graphique des KD, il est également un des joueurs avec le KD le plus élevé. De plus, nous pouvons confirmer qu'il est parmi les meilleurs joueurs du tournoi grâce au classement affiché pour chaque statistiques.

Concernant son positionnement au sein de son équipe, il n'est donc pas surprenant qu'il ait le rating, l'ADR, ratio et kast le plus élevé. En revanche son taux de headshots est le plus faible : il concentre ses tirs sur les zones de l'adversaire les plus accessibles et non pas forcément sur la tête. Il est également dernier au nombre d'assistances réalisées. Cela n'est pas étonnant car en ayant le plus grands nombre de kills de son équipe, il profite du soutien de ses partenaires et a donc peu d'occasion de leur apporter le sien, ou bien il finit ses propres kills la plupart du temps sans avoir besoin de l'aide de ses coéquipiers.

Enfin, en observant l'évolution de son rating, nous comprenons d'où vient sa considération de meilleur joueur au monde. Depuis le début de sa carrière, il a un rating moyen bien supérieur à la moyenne. Notamment depuis début 2018, où il est chaque mois très proche de l'upper fence. 

Une amélioration possible de cet onglet serait de remplacer les text input pour team et joueur et de les remplacer par un dropdown ne contenant que les valeurs des joueurs / teams présentes sur le tournoi sélectionné. Comme cette fonctionnalité n'est pas en place, vous pouvez retrouver la liste des majors, des équipes et des joueurs à l'adresse suivante : 
  https://www.hltv.org/events/archive?eventType=MAJOR




