analyse_regression_mois <- function(data) {
    data_reg_mois <- data.frame(NombreAccidents = table(data$mois)) # récupération du nombre d'accident par mois
    data_reg_mois$NombreAccidents.Var1 = as.numeric(data_reg_mois$NombreAccidents.Var1)
    model1 <- lm(data_reg_mois$NombreAccidents.Freq~data_reg_mois$NombreAccidents.Var1) # régression linéaire
    #on affiche les différents coefficients
    coef(model1)
    summary(model1)
    anova(model1)
    confint(model1) #donne intervalle de confiance à 95%

    
    p <- predict(model1, data_reg_mois, se.fit=TRUE, interval ="prediction") #les prédiction avec interval de confiance à 95%
    print(p)
    # Calcul de l'erreur quadratique moyenne (EQM)
    print(mean((data_reg_mois$NombreAccidents.Freq - p$fit)^2))

    #on afiche les différents valauers et la régression linéaire avec son intervalle de confiance
    ggplot(data_hist_mois, aes(y=NombreAccidents.Freq, x=NombreAccidents.Var1))+
    geom_point()+
    geom_smooth(colour="red", method="lm",fill="red") +
    theme_classic() +
    ylab("Nombre d'accidents") +
    xlab("Mois de l'année") +
    ggtitle("Nombre d’accidents en fonction du mois de l'année") +
    annotate("text", x = 6.5, y = 3750, label = "Régréssion linéaire avec intervalle de confiance à 95%")
}



analyse_regression_semaine <- function(data) {
    data_reg_week <- data.frame(NombreAccidents = table(data$week)) # récupération du nombre d'accident par semaine
    print(data_reg_week)
    data_reg_week$NombreAccidents.Var1 = as.numeric(data_reg_week$NombreAccidents.Var1)

    model2 <- lm(data_reg_week$NombreAccidents.Freq~data_reg_week$NombreAccidents.Var1) # régression linéaire
    #on affiche les différents coefficients
    coef(model2)
    summary(model2)
    anova(model2)
    confint(model2) #donne intervalle de confiance à 95%
    print(data_reg_week)
    
    p1 <- predict(model2, data_reg_week, se.fit=TRUE, interval ="prediction") #les prédiction avec interval de confiance à 95%
    print(p1)
    # Calcul de l'erreur quadratique moyenne (EQM)
    print(mean((data_reg_week$NombreAccidents.Freq - p1$fit)^2))

    #on afiche les différents valauers et la régression linéaire avec son intervalle de confiance
    ggplot(data_reg_week, aes(y=NombreAccidents.Freq, x=NombreAccidents.Var1))+
    geom_point()+
    geom_smooth(colour="red", se=TRUE, method="lm", fill="red") +
    theme_classic()+
    ylab("Nombre d'accidents") +
    xlab("Semaine de l'anée") +
    ggtitle("Nombre d’accidents en fonction de la semaine de l'année") +
    annotate("text", x = 25, y = 250, label = "Régréssion linéaire avec intervalle de confiance à 95%")
}