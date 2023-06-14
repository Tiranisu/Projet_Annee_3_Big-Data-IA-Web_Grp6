chi2_description_intersection_descr_grav <- function(data) {
    t <- table(data$description_intersection, data$descr_grav)
    print(t)
    khi_test <- chisq.test(t)
    print(khi_test)
    print(khi_test$p.value)
    mosaicplot(t, color = c("orange", "red", "blue", "green"), main = "Title")
}


chi2_description_intersection_descr_type_col <- function(data) {
    t <- table(data$description_intersection, data$descr_type_col)
    print(t)
    khi_test <- chisq.test(t)
    print(khi_test)
    print(khi_test$p.value)
    mosaicplot(t, color = c("orange", "red", "blue", "green"), main = "Title")
}


chi2_descr_etat_surf_descr_grav <- function(data) {
    t <- table(data$descr_etat_surf, data$descr_grav)
    print(t)
    khi_test <- chisq.test(t)
    print(khi_test)
    print(khi_test$p.value)
    mosaicplot(t, color = c("orange", "red", "blue", "green"), main = "Title")
}


chi2_descr_lum_descr_grav <- function(data) {
    t <- table(data$descr_grav, data$descr_lum)
    print(t)
    khi_test <- chisq.test(t)
    print(khi_test)
    print(khi_test$p.value)
    mosaicplot(t, color = c("orange", "red", "blue", "green"), main = "Title")
}


chi2_date_descr_grav <- function(data) {
    t <- table(data$date, data$descr_grav)
    print(t)
    khi_test <- chisq.test(t)
    print(khi_test)
    print(khi_test$p.value)
    mosaicplot(t, color = c("orange", "red", "blue", "green"), main = "Title")
}


chi2_age_descr_grav <- function(data) {
    t <- table(data$age, data$descr_grav)
    print(t)
    khi_test <- chisq.test(t)
    print(khi_test)
    print(khi_test$p.value)
    mosaicplot(t, color = c("red", "orange", "green", "black"), main = "Title")
}