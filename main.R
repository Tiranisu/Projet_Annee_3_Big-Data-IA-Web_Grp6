############################################
# Projet : Statistiques des accidents de la route
# Auteurs : Dorian TARIN, Antonin SABIRON, Mael GRELLIER-NEAU, Téo RORTAIS
# Date : 16/06/2023
############################################


#------------------------------------------------------#
#                                                      #
#         Librairies et fichiers nécessaires           #
#                                                      #
#------------------------------------------------------#

#install.packages("leaflet")
#install.packages("geojsonio")
#install.packages("htmltools")
#install.packages("mapview")

library(tidyverse)  #Pour le recode
library(chron)      #Pour le format date
library(dplyr)
library(leaflet)
library(geojsonio)
library(htmltools)
library(mapview)

library(webshot)
#webshot::install_phantomjs(force=TRUE)

data <- read.csv('data/stat_acc_V3.csv', sep=';')
villes <- read.csv('data/laposte_hexasmal.csv', sep=';')


#Pour la partie preparation 2

#CSV pour obtenir le nom de la région et du département à partir du code INSEE
#https://static.data.gouv.fr/resources/communes-de-france-base-des-codes-postaux/20200309-131459/communes-departement-region.csv
data_com_dep_reg <- read.csv('data/communes-departement-region.csv', sep=',')

#CSV pour obtenir le nombre d'habitants par région
#https://www.insee.fr/fr/statistiques/4265429?sommaire=4265511
data_reg <- read.csv('data/regions.csv', sep=';')



#------------------------------------------------------#
#                                                      #
#              Préparation des données                 #
#                                                      #
#------------------------------------------------------#
source("Preparation_data.R")


#Pour connaitres les catégorie d'une colonne
unique(data$descr_grav)
unique(data$descr_cat_veh)

#On remplace les valeurs manquantes dans la colonne "place" par 0 (qu'on considère donc piéton)
data$place[data$place=="NULL"]<-"0"

#On remplace les "NULL" de la colonne age (avec à peu près la moyenne) et on réajuste les valeurs (-14 ans)
data$age[data$age=="NULL"]<-"65"
temp_age = as.numeric(data$age)
temp_age=temp_age-14
data$age=temp_age

data$an_nais[data$an_nais=="NULL"]<-"1958"

#On corrige les problèmes de loalisation GPS grâce à une base de données de la poste
villes$latitude <- sapply(strsplit(as.character(villes$coordonnees_geographiques), ","), head, 1)
villes$longitude <- sapply(strsplit(as.character(villes$coordonnees_geographiques), ","), tail, 1)
data$latitude <- villes$latitude[match(data$id_code_insee, villes$Code_commune_INSEE)]
data$longitude <- villes$longitude[match(data$id_code_insee, villes$Code_commune_INSEE)]

preparation_variables_multimodales(data)


#On met les variables numeriques sous format numérique et date en date
data$date <- as.chron(data$date)
data$Num_Acc <- as.numeric(data$Num_Acc)
data$id_usa <- as.numeric(data$id_usa)
data$latitude <- as.numeric(data$latitude)
data$longitude <- as.numeric(data$longitude)
data$descr_cat_veh <- as.numeric(data$descr_cat_veh)
data$an_nais <- as.numeric(data$an_nais)
data$age <- as.numeric(data$age)
data$place <- as.numeric(data$place)
# data$descr_grav <- as.numeric(data$descr_grav)


#Construction des séries chronologiques sur l'évolution du nombre d'accidents par mois et par semaine en ajoutant 
#une colonne "num_mois" et "num_semaine" contenant le numéro du mois et de la semaine de l'accident
preparation_Ajout_Num_Mois_Semaines(data)


#Construire un jeu de données avec le nombre d’accidents selon la gravité pour 100.000 habitants par région re d'accident par gravité

#On commmence par enlever tous les doublons de la base de données des communes, départements et régions pour éviter les doublons
data_com_dep_reg <- data_com_dep_reg %>% group_by(code_commune_INSEE) %>% filter (! duplicated(code_commune_INSEE))

#On enlève les colonnes inutiles
data_com_dep_reg <- data_com_dep_reg[, c("code_commune_INSEE", "nom_departement", "nom_region")]
#On enlève les collones inutiles
data_reg <- data_reg[, c("REG", "PMUN")]

#On merge les deux bases de données pour avoir le nom de la région et du département à partir du code INSEE
data_total <- merge(data, data_com_dep_reg, by.x = "id_code_insee", by.y = "code_commune_INSEE")

#On crée un jeu de données avec le nombre d'accident par gravité et par région
ACP_data <- data_total %>% group_by(descr_grav, nom_region) %>% summarise(nb_acc = n()) %>% ungroup()

#On merge les deux bases de données pour avoir le nombre d'habitants par région
ACP_data <- merge(ACP_data, data_reg, by.x = "nom_region", by.y = "REG")
#On crée une colonne avec le nombre d'accident pour 100.000 habitants grace à la colonne PMUN qui contient le nombre d'habitants par région
ACP_data$nb_acc_pour_100000_hab <- (100000 * ACP_data$nb_acc) / ACP_data$PMUN

#On crée un jeu de données avec le nombre d'accident par region, utilisé pour la carte
accident_region <- data_total %>% group_by(nom_region) %>% count()
##On crée un jeu de données avec le nombre d'accident par département, utilisé pour la carte
accident_departement <- data_total %>% group_by(nom_departement) %>% count()


#On remet les 9 premiers départements manquants dans la base d'accidents
accident_departement[nrow(accident_departement) + 1,] <- list("Ain",NaN)
accident_departement[nrow(accident_departement) + 1,] <- list("Aisne",NaN)
accident_departement[nrow(accident_departement) + 1,] <- list("Allier",NaN)
accident_departement[nrow(accident_departement) + 1,] <- list("Alpes-de-Haute-Provence",NaN)
accident_departement[nrow(accident_departement) + 1,] <- list("Hautes-Alpes",NaN)
accident_departement[nrow(accident_departement) + 1,] <- list("Alpes-Maritimes",NaN)
accident_departement[nrow(accident_departement) + 1,] <- list("Ardèche",NaN)
accident_departement[nrow(accident_departement) + 1,] <- list("Ardennes",NaN)
accident_departement[nrow(accident_departement) + 1,] <- list("Ariège",NaN)



#------------------------------------------------------#
#                                                      #
#             Visualisations des données               #
#                                                      #
#------------------------------------------------------#
source("Visualisation_representation_graphique.R")
source("Visualisation_histogrammes.R")

#Pour preciser dans quel dossier on veut enrengistrer les graphs
chemin <- "img/"

#Affichage des représentations graphiques
visualisation_Nb_Acc_Athmo(chemin, data)
visualisation_Nb_Acc_Surface(chemin, data)
visualisation_Nb_Acc_Gravite(chemin, data)
visualisation_Nb_Acc_Heure(chemin, data)
visualisation_Nb_Acc_Ville(chemin, data)

#Affichage des histogrammes
visualisation_Nb_Acc_Age(chemin, data)
visualisation_Nb_Acc_Mois(chemin, data)

#Affichage des cartes
source("Visualisation_carte.R")
visualisation_carte_region(accident_region)
visualisation_carte_departement(accident_departement)
visualisation_carte_taux_acc_grave_region(data_total,accident_region)
visualisation_carte_taux_acc_grave_departement(data_total,accident_departement)



#------------------------------------------------------#
#                                                      #
#                Analyse des données                   #
#                                                      #
#------------------------------------------------------#
source("Analyse_relations_variables_qualitatives.R")
source("Analyse_regressions_linéaires.R")

#Tests d'indépendance de chi2 sur les variables qualitatives
chi2_description_intersection_descr_grav(data)
chi2_description_intersection_descr_type_col(data)
chi2_descr_etat_surf_descr_grav(data)
chi2_descr_lum_descr_grav(data)
chi2_date_descr_grav(data)
chi2_age_descr_grav(data)
chi2_id_usa_age(data)
chi2_descr_athmo_descr_grav(data)

source("Analyse_bonus.R")
#analyse_bonus()

#------------------------------------------------------#
write.csv(data_total, "export.csv", row.names=FALSE)
print("Program END")
#------------------------------------------------------#
