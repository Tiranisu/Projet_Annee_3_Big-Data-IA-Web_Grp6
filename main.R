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

data <- read.csv('stat_acc_V3.csv', sep=';')
villes <- read.csv('laposte_hexasmal.csv', sep=';')



#------------------------------------------------------#
#                                                      #
#             Visualisations des données               #
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
#Création d'une colonne mois dans le dataframe
data$mois <- month(data$date)

#Création d'une week mois dans le dataframe contenant le numéro de la semaine
data$week <- week(data$date)

# plot(data$week)
# plot(data$mois)

#Construire un jeu de données avec le nombre d’accidents selon la gravité pour 100.000 habitants par région re d'accident par gravité
data_com_dep_reg <- read.csv('communes-departement-region.csv', sep=',')
#https://www.insee.fr/fr/statistiques/4265429?sommaire=4265511
data_reg <- read.csv('regions.csv', sep=';')

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

chemin = "C:/Users/teoro/Documents/Ecole/Projet BD" #Pour preciser dans quel dossier on veut enrengistrer les graphs

#NOMBRE D'ACCIDENT EN FONCTION DES CONDITIONS ATMOSPHERIQUES
visualisation_Nb_Acc_Athmo <- function(chemin) {
  dataf1 <- data.frame(a=unique(data$descr_athmo),nb=unique(table(data$descr_athmo)))
  graph <- ggplot(dataf1 %>% arrange(nb) %>% tail(20) %>% mutate(a=factor(a, a)), aes(x=a, y=nb) ) +
    geom_bar(stat="identity", fill="#69b3a2") +
    coord_flip() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="none"
    ) +
    xlab("Conditions atmosphériques") +
    ylab("Nombre d'accidents") +
    ggtitle("Nombre d’accidents en fonction des conditions atmosphériques")
  ggsave(filename = paste(chemin, "/graph_nb_acc_athmo.png"), plot = graph, width = 8, height = 5)
}

# NOMBRE D'ACCIDENT EN FONCTION DE LA DESCRIPTION DE LA SURFACE
visualisation_Nb_Acc_Surface <- function(chemin) {
  dataf1 <- data.frame(a=unique(data$descr_etat_surf),nb=unique(table(data$descr_etat_surf)))
  graph <- ggplot(dataf1 %>% arrange(nb) %>% tail(20) %>% mutate(a=factor(a, a)), aes(x=a, y=nb) ) +
    geom_bar(stat="identity", fill="#69b3a2") +
    coord_flip() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="none"
    ) +
    xlab("Nombre d'accidents") +
    ylab("Description de la surface") +
    ggtitle("Nombre d’accidents en fonction de la description de la surface")
  ggsave(filename = paste(chemin, "/graph_nb_acc_surface.png"), plot = graph, width = 8, height = 5)
}

#NOMBRE D'ACCIDENT SELON LA GRAVITE
visualisation_Nb_Acc_Gravite <- function(chemin) {
  grav = c("Indemne", "Blessé léger", "Blessé hospitalisé", "Tué")
  count.data <- data.frame(class = grav, n = unique(table(data$descr_grav)), prop = unique(table(data$descr_grav)))
  
  #Ajouter la position de l'étiquette
  count.data <- count.data %>%
    arrange(desc(class)) %>%
    mutate(lab.ypos = cumsum(prop) - 0.5*prop)
  mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
  
  #Diagramme camembert
  graph <- ggplot(count.data, aes(x = "", y = prop, fill = class)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text(aes(y = lab.ypos, label = prop), color = "white")+
    scale_fill_manual(values = mycols) +
    theme_void() +
    ggtitle("Nombre d’accidents en fonction de la gravité")
  ggsave(filename = paste(chemin, "/graph_nb_acc_gravite_pie.png"), plot = graph, width = 8, height = 5)
  
  #Diagramme donut
  graph <- ggplot(count.data, aes(x = 2, y = prop, fill = class)) +
    geom_bar(stat = "identity", color = "white") +
    coord_polar(theta = "y", start = 0)+
    geom_text(aes(y = lab.ypos, label = prop), color = "white")+
    scale_fill_manual(values = mycols) +
    theme_void() +
    xlim(0.5, 2.5) +
    ggtitle("Nombre d’accidents en fonction de la gravité")
  ggsave(filename = paste(chemin, "/graph_nb_acc_gravite_donut.png"), plot = graph, width = 8, height = 5)
}

#NOMBRE D'ACCIDENTS PAR TRANCHES D'HEURE
visualisation_Nb_Acc_Heure <- function(chemin) {
  data_hist_time <- data.frame(heure = unique(hour(data$date)), NombreAccidents = table(hour(data$date)))
  data_hist_time$NombreAccidents.Var1 <- fct_recode(data_hist_time$NombreAccidents.Var1, 
                                                    "[0;1]" = "0",                                                                              
                                                    "[1;2]" = "1",                                         
                                                    "[2;3]" = "2",                                                                                     
                                                    "[3;4]" = "3",  
                                                    "[4;5]" = "4",                                                                                     
                                                    "[5;6]" = "5",                                                                      
                                                    "[6;7]" = "6",                                                                         
                                                    "[7;8]" = "7",                                                                      
                                                    "[8;9]" = "8",                                                            
                                                    "[9;10]" = "9",                                                                           
                                                    "[10;11]" = "10",                                                                  
                                                    "[11;12]" = "11", 
                                                    "[12;13]" = "12",                                                                             
                                                    "[13;14]" = "13",                                        
                                                    "[14;15]" = "14",                                                                                    
                                                    "[15;16]" = "15", 
                                                    "[16;17]" = "16",                                                                                    
                                                    "[17;18]" = "17",                                                                     
                                                    "[18;19]" = "18",                                                                        
                                                    "[19;20]" = "19",                                                                     
                                                    "[20;21]" = "20",                                                           
                                                    "[21;22]" = "21",                                                                          
                                                    "[22;23]" = "22",                                                                  
                                                    "[23;00]" = "23")
  graph <- ggplot(data_hist_time, aes(x = NombreAccidents.Var1, y = NombreAccidents.Freq)) + coord_flip() + geom_bar(stat = "identity") + xlab("Tranche horraire") + ylab("Nombre d’accidents") + ggtitle("Nombre d’accidents par tranches d’heure") + theme(panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank(),legend.position="none") + geom_bar(stat="identity", fill="#69b3a2")
  ggsave(filename = paste(chemin, "/graph_nb_acc_heure.png"), plot = graph, width = 8, height = 5)
}

# NOMBRE D'ACCIDENT PAR VILLE
visualisation_Nb_Acc_Ville <- function(chemin) {
  table_valeurs <- table(data$ville)
  df_resultat <- as.data.frame(table_valeurs)
  graph <- ggplot(df_resultat %>% arrange(Freq) %>% tail(20) %>% mutate(a=factor(Var1, Var1)), aes(x=a, y=Freq) ) +
    geom_bar(stat="identity", fill="#69b3a2") +
    coord_flip() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="none"
    ) +
    xlab("Nombre d’accidents") +
    ylab("Ville / arrondissement") +
    ggtitle("Nombre d’accidents par Ville / arrondissement")
  ggsave(filename = paste(chemin, "/graph_nb_acc_ville.png"), plot = graph, width = 8, height = 5)
}

#HISTOGRAMME TRANCHE D'AGE
visualisation_Nb_Acc_Age <- function(chemin) {
  tranches <- cut(data$age, breaks = seq(0, max(data$age), by = 9), include.lowest = TRUE)
  data_hist_age <- data.frame(TrancheAge = levels(tranches), NombreAccidents = table(tranches))
  graph <- ggplot(data_hist_age, aes(x = NombreAccidents.tranches, y = NombreAccidents.Freq)) + geom_bar(stat = "identity") + xlab("Tranche d'âge") + ylab("Nombre d'accidents") + ggtitle("Quantité d'accidents en fonction des tranches d'âge")
  ggsave(filename = "C:/Users/teoro/Documents/Ecole/Projet BD/graph_hist_age.png", plot = graph, width = 8, height = 5)
}

#HISTOGRAMME MOIS DE L'ANNEE
visualisation_Nb_Acc_Mois <- function(chemin) {
  #data$mois <- month(data$date)       # !!!! Faire gaffe, normalement deja faite 
  data_hist_mois <- data.frame(NombreAccidents = table(data$mois))
  data_hist_mois$NombreAccidents.Var1 <- fct_recode(data_hist_mois$NombreAccidents.Var1, 
                                                    "Janvier" = "1",                                                                               
                                                    "Fevrier" = "2",                                          
                                                    "Mars" = "3",                                                                                      
                                                    "Avril" = "4",                                                                                      
                                                    "Mai" = "5",                                                                       
                                                    "Juin" = "6",                                                                          
                                                    "Juillet" = "7",                                                                       
                                                    "Aout" = "8",                                                             
                                                    "Septembre" = "9",                                                                            
                                                    "Octobre" = "10",                                                                   
                                                    "Novembre" = "11",                                                                                      
                                                    "Decembre" = "12")
  graph <- ggplot(data_hist_mois, aes(x = NombreAccidents.Var1, y = NombreAccidents.Freq)) + geom_bar(stat = "identity") + xlab("Mois") + ylab("Nombre d'accidents") + ggtitle("Quantité d'accidents en fonction du mois de l'année")
  ggsave(filename = paste(chemin, "/graph_hist_month.png"), plot = graph, width = 8, height = 5)
}

#------------------------------------------------------#
#                                                      #
#                Analyse des données                   #
#                                                      #
#------------------------------------------------------#


######################CARTE DE FRANCE######################
install.packages("leaflet")
install.packages("geojsonio")
install.packages("htmltools")
library(leaflet)
library(dplyr)
library(geojsonio)
library(htmltools)
#https://rstudio.github.io/leaflet/choropleths.html
carte_r <- geojsonio::geojson_read("regions.geojson", what = "sp")
carte_r = carte_r[c(-14),]
carte_r$accident<-0

carte_r$accident<-accident_region$n[match(tolower(accident_region$nom_region),tolower(carte_r$nom))]


pal_r <- colorBin(
  "Reds", carte_r$accident, 9
)

labels_r <- sprintf(
  "<strong>%s</strong><br/>%g",
  carte_r$nom, carte_r$accident
) %>% lapply(htmltools::HTML)

map1 <- leaflet(carte_r) %>%
  setView(4,47,zoom=6)%>%
  addPolygons(
  fillColor = ~pal_r(accident),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlightOptions = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label=labels_r)%>%
  addLegend(pal = pal_r, values = ~nom, opacity = 0.7, title = "Accidents/région",
            position = "bottomright")




carte_d <- geojsonio::geojson_read("departements.geojson", what = "sp")
carte_d$accident<-0

carte_d$accident<-accident_departement$n[match(tolower(carte_d$nom),tolower(accident_departement$nom_departement))]

bins <- c(0, 10, 20, 50, 100, 200, 500, 10000, Inf)
pal_d <- colorBin(c("#ffffff","#f5ebec","#fad2d6","#ffbdc3","#fa8e98","#ff7380","#ff5959","#ff1717","#9e0202"), domain = carte_d$accident, bins = bins)


labels_d <- sprintf(
  "<strong>%s</strong><br/>%g",
  carte_d$nom, carte_d$accident
) %>% lapply(htmltools::HTML)

map2 <- leaflet(carte_d) %>%
  setView(4,47,zoom=6)%>%
  addPolygons(
  fillColor = ~pal_d(accident),
  weight = 1.2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlightOptions = highlightOptions(
    weight = 2,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label=labels_d)%>%
  addLegend(pal = pal_d, values = ~accident, opacity = 0.7, title = "Accidents/département",
            position = "bottomright")

mapshot(map1, file = paste0(getwd(), "/accident_region.png"))
mapshot(map2, file = paste0(getwd(), "/accident_departement.png"))


########################################################################################


###################### TEST CHI2 ######################
###################### TEST CHI2 ######################
###################### TEST CHI2 ######################

t <- table(data$descr_cat_veh, data$descr_grav)
t
khi_test <- chisq.test(t)
khi_test
khi_test$p.value

###################### TEST CHI2 ######################
###################### TEST CHI2 ######################
###################### TEST CHI2 ######################