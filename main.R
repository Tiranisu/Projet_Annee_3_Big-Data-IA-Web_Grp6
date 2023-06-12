data = read.csv('stat_acc_V3.csv', sep=';')

#Création d'une colonne mois dans le dataframe
data$mois = month(data$date)

#Création d'une week mois dans le dataframe contenant le numéro de la semaine
data$week = week(data$date)

plot(data$week)
