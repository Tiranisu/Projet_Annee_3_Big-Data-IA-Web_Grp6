analyse_bonus <- function() {
    #On prépare les données
    ACP_data$nom_region <- as.numeric(factor(ACP_data$nom_region))
    ACP_data$descr_grav <- as.numeric(ACP_data$descr_grav)
    ACP_data$nb_acc <- as.numeric(ACP_data$nb_acc)
    ACP_data$nb_acc_pour_100000_hab <- as.numeric(ACP_data$nb_acc_pour_100000_hab)
    ACP_data$PMUN <- as.numeric(ACP_data$PMUN)

    #On installe factoextra si besoin
    if(!require(devtools)) install.packages("devtools")
    devtools::install_github("kassambara/factoextra")
    library("factoextra")

    #On affiche l'inertie de chaque nouvelle dimension
    res.pca <- prcomp(ACP_data,  scale = TRUE)
    get_eig(res.pca)

    #On affiche l'eboulis des valeurs propre 
    fviz_eig(res.pca)
    fviz_eig(res.pca, addlabels=TRUE, hjust = -0.3)
    fviz_eig(res.pca, addlabels=TRUE, hjust = -0.3) + ylim(0, 50)

    #Représentation des variables sur le cercle des corrélations
    fviz_pca_var(res.pca, col.var = "blue", axes = c(1, 2))
    fviz_pca_var(res.pca, col.var = "blue", axes = c(2, 3))
    fviz_pca_var(res.pca, col.var = "blue", axes = c(1, 3))

    #Affichage des coefficients de la combinaison linéaire des nouvelles dimension
    coefficients <- res.pca$rotation
    print(coefficients)
}

