############################################
# Projet : Statistiques des accidents de la route
# Auteurs : Dorian TARIN, Antonin SABIRON, Mael GRELLIER-NEAU, Téo RORTAIS
# Date : 17/06/2023
# Version : 1.0
############################################


#------------------------------------------------------#
#                                                      #
#             Visualisations des données               #
#                                                      #
#------------------------------------------------------#

library(tidyverse)  #Pour le recode
library(chron)      #Pour le format date
library(dplyr)

data <- read.csv('data/stat_acc_V3.csv', sep=';')
villes <- read.csv('data/laposte_hexasmal.csv', sep=';')



#------------------------------------------------------#
#                                                      #
#              Préparation des données                 #
#                                                      #
#------------------------------------------------------#

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

#Codage des description de la gravité, on passe d'une chaine de charactere à un chiffre : ACHTUNG! Aussi considéré comme char
data$descr_grav <- fct_recode(data$descr_grav, "0" = "Indemne", "1" = "Blessé léger", "2" = "Blessé hospitalisé", "3" = "Tué")

#Codage des descriptions des catégorie des véhicules, on passe d'une chaine de charactere à un chiffre :  ACHTUNG! Aussi considéré comme char
data$descr_cat_veh <- fct_recode(data$descr_cat_veh, 
  "0" = "PL seul > 7,5T",                                                                               
  "1" = "VU seul 1,5T <= PTAC <= 3,5T avec ou sans remorque ",                                          
  "2" = "VL seul",                                                                                      
  "3" = "Autocar",                                                                                      
  "4" = "PL > 3,5T + remorque",                                                                       
  "5" = "Cyclomoteur <50cm3",                                                                          
  "6" = "Motocyclette > 125 cm3",                                                                       
  "7" = "Tracteur routier + semi-remorque",                                                             
  "8" = "Tracteur agricole",                                                                            
  "9" = "PL seul 3,5T <PTCA <= 7,5T",                                                                   
  "10" = "Autobus",                                                                                      
  "11" = "Scooter > 50 cm3 et <= 125 cm3",                                                               
  "12" = "Train",                                                                                        
  "13" = "Scooter > 125 cm3",                                                                            
  "14" = "Scooter < 50 cm3",                                                                             
  "15" = "Voiturette (Quadricycle à moteur carrossé) (anciennement \"voiturette ou tricycle à moteur\")",
  "16" = "Autre véhicule",                                                                               
  "17" = "Bicyclette",                                                                                   
  "18" = "Motocyclette > 50 cm3 et <= 125 cm3",                                                          
  "19" = "Engin spécial",                                                                                
  "20" = "Quad lourd > 50 cm3 (Quadricycle à moteur non carrossé)",                                      
  "21" = "Tramway",                                                                                      
  "22" = "Tracteur routier seul",                                                                        
  "23" = "Quad léger <= 50 cm3 (Quadricycle à moteur non carrossé)") 

#On met les variables numeriques sous format numérique et date en date
#ACHTUNG ! Peut etre fais buguer la suite du code si vous aviez considéré vos valeurs en string (elle ne le sont plus toute)
data$date <- as.chron(data$date)
data$Num_Acc <- as.numeric(data$Num_Acc)
data$id_usa <- as.numeric(data$id_usa)
data$latitude <- as.numeric(data$latitude)
data$longitude <- as.numeric(data$longitude)
data$descr_cat_veh <- as.numeric(data$descr_cat_veh)
data$an_nais <- as.numeric(data$an_nais)
data$age <- as.numeric(data$age)
data$place <- as.numeric(data$place)
data$descr_grav <- as.numeric(data$descr_grav)


#Construction du jeu de données avec le nombre d'accidents selon la gravité p
#Création d'une colonne mois dans la dataframe
data$mois <- month(data$date)

#Création d'une week mois dans le dataframe contenant le numéro de la semaine
data$week <- week(data$date)

# plot(data$week)
# plot(data$mois)

#Construire un jeu de données avec le nombre d’accidents selon la gravité pour 100.000 habitants par région re d'accident par gravité
data_com_dep_reg <- read.csv('data/communes-departement-region.csv', sep=',')
#https://www.insee.fr/fr/statistiques/4265429?sommaire=4265511
data_reg <- read.csv('data/regions.csv', sep=';')

data_com_dep_reg <- data_com_dep_reg %>% group_by(code_commune_INSEE) %>% filter (! duplicated(code_commune_INSEE))

data_com_dep_reg <- data_com_dep_reg[, c("code_commune_INSEE", "nom_departement", "nom_region")]
data_reg <- data_reg[, c("REG", "PMUN")]

data_total <- merge(data, data_com_dep_reg, by.x = "id_code_insee", by.y = "code_commune_INSEE")

ACP_data <- data_total %>% group_by(descr_grav, nom_region) %>% summarise(nb_acc = n()) %>% ungroup()
accident_region <- data_total %>% group_by(nom_region) %>% count()
accident_departement <- data_total %>% group_by(nom_departement) %>% count()

ACP_data <- merge(ACP_data, data_reg, by.x = "nom_region", by.y = "REG")
ACP_data$nb_acc_pour_100000_hab <- (100000 * ACP_data$nb_acc) / ACP_data$PMUN


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
chemin = "C:/oui" #Pour preciser dans quel dossier on veut enrengistrer les graphs

visualisation_Nb_Acc_Athmo(chemin, data)
visualisation_Nb_Acc_Surface(chemin, data)
visualisation_Nb_Acc_Gravite(chemin, data)
visualisation_Nb_Acc_Heure(chemin, data)
visualisation_Nb_Acc_Ville(chemin, data)

visualisation_Nb_Acc_Age(chemin, data)
visualisation_Nb_Acc_Mois(chemin, data)

#------------------------------------------------------#
#                                                      #
#                Analyse des données                   #
#                                                      #
#------------------------------------------------------#
source("Analyse_relations_variables_qualitatives.R")

# chi2_description_intersection_descr_grav(data)
# chi2_description_intersection_descr_type_col(data)
# chi2_descr_etat_surf_descr_grav(data)
# chi2_descr_lum_descr_grav(data)
# chi2_date_descr_grav(data)
# chi2_age_descr_grav(data)


######################CARTE DE FRANCE######################
install.packages("leaflet")
install.packages("geojsonio")
install.packages("htmltools")
library(leaflet)
library(dplyr)
library(geojsonio)
library(htmltools)

source("Visualisation_carte.R")
visualisation_carte_region()
visualisation_carte_departement()


print("Program END")
########################################################################################