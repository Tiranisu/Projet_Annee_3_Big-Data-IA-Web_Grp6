#HISTOGRAMME TRANCHE D'AGE
visualisation_Nb_Acc_Age <- function(chemin, data) {
  tranches <- cut(data$age, breaks = seq(0, max(data$age), by = 9), include.lowest = TRUE)
  data_hist_age <- data.frame(TrancheAge = levels(tranches), NombreAccidents = table(tranches))
  graph <- ggplot(data_hist_age, aes(x = NombreAccidents.tranches, y = NombreAccidents.Freq)) + geom_bar(stat = "identity") + xlab("Tranche d'âge") + ylab("Nombre d'accidents") + ggtitle("Quantité d'accidents en fonction des tranches d'âge")
  ggsave(filename = paste(chemin, "/graph_hist_age.png"), plot = graph, width = 8, height = 5)
}

#HISTOGRAMME MOIS DE L'ANNEE
visualisation_Nb_Acc_Mois <- function(chemin, data) {
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