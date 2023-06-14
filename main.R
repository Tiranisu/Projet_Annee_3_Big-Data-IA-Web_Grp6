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


data_com_dep_reg <- data_com_dep_reg[, c("code_commune_INSEE", "nom_departement", "nom_region")]
data_reg <- data_reg[, c("REG", "PMUN")]

data_total <- merge(data, data_com_dep_reg, by.x = "id_code_insee", by.y = "code_commune_INSEE")

ACP_data <- data_total %>% group_by(descr_grav, nom_region) %>% summarise(nb_acc = n()) %>% ungroup()
accident_region <- data_total %>% group_by(nom_region) %>% count()
accident_departement <- data_total %>% group_by(nom_departement) %>% count()

ACP_data <- merge(ACP_data, data_reg, by.x = "nom_region", by.y = "REG")
ACP_data$nb_acc_pour_100000_hab <- (100000 * ACP_data$nb_acc) / ACP_data$PMUN



#------------------------------------------------------#
#                                                      #
#             Visualisations des données               #
#                                                      #
#------------------------------------------------------#

# Nombre d’accidents par ville

table_valeurs <- table(data$ville)

# Conversion du tableau en dataframe
df_resultat <- as.data.frame(table_valeurs)

# print(df_resultat)

df_resultat %>%
  arrange(Freq) %>%
  tail(20) %>%
  mutate(a=factor(Var1, Var1)) %>%
  ggplot( aes(x=a, y=Freq) ) +
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


# Nombre d’accidents en fonction de la description de la surface

# print(unique(data$descr_etat_surf))
# print(unique(table(data$descr_etat_surf)))

dataf1 <- data.frame(a=unique(data$descr_etat_surf),nb=unique(table(data$descr_etat_surf)))

dataf1 %>%
  arrange(nb) %>%
  tail(20) %>%
  mutate(a=factor(a, a)) %>%
  ggplot( aes(x=a, y=nb) ) +
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


#Nombre d’accidents selon la gravité

grav = c("Indemne", "Blessé léger", "Blessé hospitalisé", "Tué")

count.data <- data.frame(
  class = grav,
  n = unique(table(data$descr_grav)),
  prop = unique(table(data$descr_grav))
)
count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
count.data

mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")

#diagramme camembert
ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()

#diagramme donut
ggplot(count.data, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)


#Nombre d’accidents en fonction des conditions atmosphériques

dataf1 <- data.frame(a=unique(data$descr_athmo),nb=unique(table(data$descr_athmo)))

dataf1 %>%
  arrange(nb) %>%
  tail(20) %>%
  mutate(a=factor(a, a)) %>%
  ggplot( aes(x=a, y=nb) ) +
  geom_bar(stat="identity", fill="#69b3a2") +
  coord_flip() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("Nombre d'accidents") +
  ylab("Conditions atmosphériques") +
  ggtitle("Nombre d’accidents en fonction des conditions atmosphériques")



#Visualisation - Histogrammes - Quantité d’accidents en fonction des tranches d’âges
tranches <- cut(data$age, breaks = seq(0, max(data$age), by = 9), include.lowest = TRUE)
data_hist_age <- data.frame(TrancheAge = levels(tranches), NombreAccidents = table(tranches))
ggplot(data_hist_age, aes(x = NombreAccidents.tranches, y = NombreAccidents.Freq)) + geom_bar(stat = "identity") + xlab("Tranche d'âge") + ylab("Nombre d'accidents") + ggtitle("Quantité d'accidents en fonction des tranches d'âge")

#Visualisation - Histogrammes - Quantité d'accidents en fonction du mois de l'année
data_hist_mois <- data.frame(NombreAccidents = table(data$mois))
data_hist_mois$NombreAccidents.Var1 <- fct_recode(data_hist_mois$NombreAccidents.Var1, 
                                 "Janvier" = "1",                                                                               
                                 "Fevrier" = "2",                                          
                                 "Mars" = "3",                                                                                      
                                 "Avril" = "4",                                                                                      
                                 "Mai" = "5",                                                                       
                                 "Juin." = "6",                                                                          
                                 "Juillet" = "7",                                                                       
                                 "Aout" = "8",                                                             
                                 "Septembre" = "9",                                                                            
                                 "Octobre" = "10",                                                                   
                                 "Novembre" = "11",                                                                                      
                                 "Decembre" = "12")
ggplot(data_hist_mois, aes(x = NombreAccidents.Var1, y = NombreAccidents.Freq)) + geom_bar(stat = "identity") + xlab("Mois") + ylab("Nombre d'accidents") + ggtitle("Quantité d'accidents en fonction du mois de l'année")



#------------------------------------------------------#
#                                                      #
#                Analyse des données                   #
#                                                      #
#------------------------------------------------------#


######################CRASH TEST ######################
######################CRASH TEST ######################
######################CRASH TEST ######################
install.packages("leaflet")
install.packages("geojsonio")
library(leaflet)
library(dplyr)
library(geojsonio)
#https://rstudio.github.io/leaflet/choropleths.html
carte <- geojsonio::geojson_read("regions.geojson", what = "sp")
names(carte)
#accident_region = data %>% group_by(region) %>% count()
carte = carte[c(-14),]

carte$accident<-0
print(carte$nom)
print(accident_region$nom_region)
carte$accident<-accident_region$n[match(tolower(accident_region$nom_region),tolower(carte$nom))]


pal <- colorBin(
  "YlOrRd", domain = carte$accident
)

m <- leaflet(carte)

m %>% addPolygons(
  fillColor = ~pal(accident),
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
    bringToFront = TRUE))
######################CRASH TEST ######################
######################CRASH TEST ######################
######################CRASH TEST ######################

