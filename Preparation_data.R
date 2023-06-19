preparation_variables_multimodales <- function(data) {
    #Codage des description de la gravité, on passe d'une chaine de charactere à un chiffre : ACHTUNG! Aussi considéré comme char
    data$descr_grav <<- fct_recode(data$descr_grav, "0" = "Indemne", "1" = "Blessé léger", "2" = "Blessé hospitalisé", "3" = "Tué")

    #Codage des descriptions des catégorie des véhicules, on passe d'une chaine de charactere à un chiffre :  ACHTUNG! Aussi considéré comme char
    data$descr_cat_veh <<- fct_recode(data$descr_cat_veh, 
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

    data$descr_agglo <<- fct_recode(data$descr_agglo,
    "0"="Hors agglomération",
    "1"="En agglomération")

    data$descr_athmo <<- fct_recode(data$descr_athmo,
    "0"="Autre",
    "1"="Brouillard – fumée",
    "2"="Neige – grêle",
    "3"="Normale",
    "4"="Pluie forte",
    "5"="Pluie légère",
    "6"="Temps couvert",
    "7"="Temps éblouissant",
    "8"="Vent fort – tempête")

    data$descr_lum <<- fct_recode(data$descr_lum,
    "0"="Crépuscule ou aube",
    "1"="Nuit avec éclairage public allumé",
    "2"="Nuit avec éclairage public non allumé",
    "3"="Nuit sans éclairage public",
    "4"="Plein jour")

    data$descr_etat_surf <<- fct_recode(data$descr_etat_surf,
    "0"="Autre",
    "1"="Boue",
    "2"="Corps gras – huile",
    "3"="Enneigée",
    "4"="Flaques",
    "5"="Inondée",
    "6"="Mouillée",
    "7"="Normale",
    "8"="Verglacée")

    data$description_intersection <<- fct_recode(data$description_intersection,
    "0"="Autre intersection",
    "1"="Giratoire",
    "2"="Hors intersection",
    "3"="Intersection à plus de 4 branches",
    "4"="Intersection en T",
    "5"="Intersection en X",
    "6"="Intersection en Y",
    "7"="Passage à niveau",
    "8"="Place")

    data$descr_dispo_secu <<- fct_recode(data$descr_dispo_secu,
    "0"="Autre - Non déterminable",
    "1"="Autre - Non utilisé",
    "2"="Autre - Utilisé",
    "3"="Présence de ceinture de sécurité non utilisée ",
    "4"="Présence dispositif enfant - Utilisation non déterminable",
    "5"="Présence d'un casque - Utilisation non déterminable",
    "6"="Présence d'un casque non utilisé ",
    "7"="Présence d'un dispositif enfant non utilisé",
    "8"="Présence d'un équipement réfléchissant non utilisé",
    "9"="Présence d'une ceinture de sécurité - Utilisation non déterminable",
    "10"="Présence équipement réfléchissant - Utilisation non déterminable",
    "11"="Utilisation d'un casque ",
    "12"="Utilisation d'un dispositif enfant",
    "13"="Utilisation d'un équipement réfléchissant ",
    "14"="Utilisation d'une ceinture de sécurité ")

    data$descr_motif_traj <<- fct_recode(data$descr_motif_traj,
    "0"="Autre",
    "1"="Courses – achats",
    "2"="Domicile – école",
    "3"="Domicile – travail",
    "4"="Promenade – loisirs",
    "5"="Utilisation professionnelle")

    data$descr_type_col <<- fct_recode(data$descr_type_col,
    "0"="Autre collision",
    "1"="Deux véhicules - Frontale",
    "2"="Deux véhicules – Par l’arrière",
    "3"="Deux véhicules – Par le coté",
    "4"="Sans collision",
    "5"="Trois véhicules et plus – Collisions multiples",
    "6"="Trois véhicules et plus – En chaîne")

}

preparation_Ajout_Num_Mois_Semaines <- function(data) {
    #Création d'une colonne mois dans la dataframe
    data$mois <<- month(data$date)

    #Création d'une week mois dans le dataframe contenant le numéro de la semaine
    data$week <<- week(data$date)
}