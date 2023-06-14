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
}



preparation_Ajout_Num_Mois_Semaines <- function(data) {
    #Création d'une colonne mois dans la dataframe
    data$mois <<- month(data$date)

    #Création d'une week mois dans le dataframe contenant le numéro de la semaine
    data$week <<- week(data$date)
}