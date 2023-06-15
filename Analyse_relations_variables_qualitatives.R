chi2_description_intersection_descr_grav <- function(data) {
    t <- table(data$description_intersection, data$descr_grav)
    # print(t)
    khi_test <- chisq.test(t)
    print(khi_test)
    print(khi_test$p.value)
    
    png("img/chi2_description_intersection_descr_grav.png")
    mosaicplot(t, color = c("red", "orange", "green", "black"), main = "Relation entre la description des intersections 
    et la gravité des accidents :", las = 3)
    dev.off()
}


chi2_description_intersection_descr_type_col <- function(data) {
    t <- table(data$description_intersection, data$descr_type_col)
    # print(t)
    khi_test <- chisq.test(t)
    print(khi_test)
    print(khi_test$p.value)

    png("img/chi2_description_intersection_descr_type_col.png")
    mosaicplot(t, color = c("red", "orange", "green", "black"), main = "Relation entre la description des intersections 
    et la description du type de colision :", las = 3)
    dev.off()
}


chi2_descr_etat_surf_descr_grav <- function(data) {
    t <- table(data$descr_etat_surf, data$descr_grav)
    # print(t)
    khi_test <- chisq.test(t)
    print(khi_test)
    print(khi_test$p.value)
    
    png("img/chi2_descr_etat_surf_descr_grav.png")
    mosaicplot(t, color = c("red", "orange", "green", "black"), main = "Relation entre la description de l'état de la surface 
    et la gravité des accidents :", las = 3)
    dev.off()
}


chi2_descr_lum_descr_grav <- function(data) {
    t <- table(data$descr_grav, data$descr_lum)
    # print(t)
    khi_test <- chisq.test(t)
    print(khi_test)
    print(khi_test$p.value)
    
    png("img/chi2_descr_lum_descr_grav.png")
    mosaicplot(t, color = c("red", "orange", "green", "black"), main = "Relation entre les conditions d'éclairage 
    et la gravité des accidents :", las = 3)
    dev.off()
}


chi2_date_descr_grav <- function(data) {
    t <- table(data$date, data$descr_grav)
    # print(t)
    khi_test <- chisq.test(t)
    print(khi_test)
    print(khi_test$p.value)
    
    # png("img/chi2_date_descr_grav.png")
    # mosaicplot(t, color = c("red", "orange", "green", "black"), main = "Relation entre la date 
    # et la gravité des accidents :", las = 3)
    # dev.off()
}


chi2_age_descr_grav <- function(data) {
    t <- table(data$age, data$descr_grav)
    # print(t)
    khi_test <- chisq.test(t)
    print(khi_test)
    print(khi_test$p.value)

    png("img/chi2_age_descr_grav.png")
    mosaicplot(t, color = c("red", "orange", "green", "black"), main = "Relation entre l'age et la gravité des accidents :", las = 3)
    dev.off()
}


chi2_id_usa_age <- function(data) {
    t <- table(data$id_usa, data$age)
    # print(t)
    khi_test <- chisq.test(t)
    print(khi_test)
    print(khi_test$p.value)

    # png("img/chi2_id_usa_age.png")
    # mosaicplot(t, color = c("red", "orange", "green", "black"), main = "Relation entre l'age et l'identifiant de l'usager :", las = 3)
    # dev.off()
}


chi2_descr_athmo_descr_grav <- function(data) {
    t <- table(data$descr_athmo, data$descr_grav)
    # print(t)
    khi_test <- chisq.test(t)
    print(khi_test)
    print(khi_test$p.value)

    png("img/chi2_descr_athmo_descr_grav.png")
    mosaicplot(t, color = c("red", "orange", "green", "black"), main = "Relation entre les conditions atmosphériques 
    et la gravité des accidents :", las = 3)
    dev.off()
}