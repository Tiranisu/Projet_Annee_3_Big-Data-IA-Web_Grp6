#NOMBRE D'ACCIDENT EN FONCTION DES CONDITIONS ATMOSPHERIQUES
visualisation_Nb_Acc_Athmo <- function(chemin, data) {
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
visualisation_Nb_Acc_Surface <- function(chemin, data) {
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
visualisation_Nb_Acc_Gravite <- function(chemin, data) {
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
visualisation_Nb_Acc_Heure <- function(chemin, data) {
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
visualisation_Nb_Acc_Ville <- function(chemin, data) {
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
