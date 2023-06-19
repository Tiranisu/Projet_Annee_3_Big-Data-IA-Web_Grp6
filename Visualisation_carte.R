#https://rstudio.github.io/leaflet/choropleths.html

visualisation_carte_region <- function(accident_region) {
    carte_r <- geojsonio::geojson_read("data/regions.geojson", what = "sp")
    carte_r = carte_r[c(-14),]
    carte_r$accident<-0

    carte_r$accident<-accident_region$n[match(tolower(carte_r$nom),tolower(accident_region$nom_region))]


    pal_r <- colorBin(
    "Reds", carte_r$accident, 9
    )

    labels_r <- sprintf(
    "<strong>%s</strong><br/>%g",
    carte_r$nom, carte_r$accident
    ) %>% lapply(htmltools::HTML)

    map1 <- leaflet(carte_r) %>%
    setView(4,47,zoom=6)%>%
    addPolygons(
    fillColor = ~pal_r(accident),
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
        bringToFront = TRUE),
    label=labels_r)%>%
    addLegend(pal = pal_r, values = ~nom, opacity = 0.7, title = "Accidents/région",
                position = "bottomright")

    mapshot(map1, file = paste0(getwd(), "/accident_region.png"))
    map1
}


visualisation_carte_departement <- function(accident_departement) {
    carte_d <- geojsonio::geojson_read("data/departements.geojson", what = "sp")
    carte_d$accident<-0

    carte_d$accident<-accident_departement$n[match(tolower(carte_d$nom),tolower(accident_departement$nom_departement))]

    bins <- c(0, 10, 20, 50, 100, 200, 500, 10000, Inf)
    pal_d <- colorBin(c("#ffffff","#f5ebec","#fad2d6","#ffbdc3","#fa8e98","#ff7380","#ff5959","#ff1717","#9e0202"), domain = carte_d$accident, bins = bins)


    labels_d <- sprintf(
    "<strong>%s</strong><br/>%g",
    carte_d$nom, carte_d$accident
    ) %>% lapply(htmltools::HTML)

    map2 <- leaflet(carte_d) %>%
    setView(4,47,zoom=6)%>%
    addPolygons(
    fillColor = ~pal_d(accident),
    weight = 1.2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
    label=labels_d)%>%
    addLegend(pal = pal_d, values = ~accident, opacity = 0.7, title = "Accidents/département",
                position = "bottomright")

    mapshot(map2, file = paste0(getwd(), "/accident_departement.png"))
    map2
}


visualisation_carte_taux_acc_grave_region <- function(data_totall,accident_region) {
#On récupère les accidents considérés comme graves
taux_accident_grave_region <- data_totall[data_totall$descr_grav=="2"|data_totall$descr_grav=="3",] %>% group_by(nom_region) %>% count()

#On trie dans l'ordre des noms des régions pour que les calculs après soient corrects
taux_accident_grave_region<-taux_accident_grave_region[order(taux_accident_grave_region$nom_region),]
accident_region<-accident_region[order(accident_region$nom_region),]

#On calcule les taux (il faut pour ça que dans les deux tableaux la région soient sur la même ligne, d'où le tri précédent)
taux_accident_grave_region$n=taux_accident_grave_region$n/accident_region$n


carte_r2 <- geojsonio::geojson_read("data/regions.geojson", what = "sp")
carte_r2 = carte_r2[c(-14),]
carte_r2$accident<-0

carte_r2$accident<-taux_accident_grave_region$n[match(tolower(carte_r2$nom),tolower(taux_accident_grave_region$nom_region))]


pal_r <- colorBin(
  "Reds", carte_r2$accident, 10
)

labels_r <- sprintf(
  "<strong>%s</strong><br/>%g",
  carte_r2$nom, carte_r2$accident
) %>% lapply(htmltools::HTML)

map3 <- leaflet(carte_r2) %>%
  setView(4,47,zoom=6)%>%
  addPolygons(
    fillColor = ~pal_r(accident),
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
      bringToFront = TRUE),
    label=labels_r)%>%
  addLegend(pal = pal_r, values = ~nom, opacity = 0.7, title = "Taux accidents graves/région",
            position = "bottomright")

mapshot(map3, file = paste0(getwd(), "/taux_accidents_graves_region.png"))
map3

}

visualisation_carte_taux_acc_grave_departement <- function(data_totall,accident_departement) {
#On récupère les accidents considérés comme graves
taux_accident_grave_departement <- data_totall[data_totall$descr_grav=="2"|data_totall$descr_grav=="3",] %>% group_by(nom_departement) %>% count()

#On rajoute les départements manquantes (que les tableaux aient le même nombre de lignes)
taux_accident_grave_departement[nrow(taux_accident_grave_departement) + 1,] <- list("Ain",NA)
taux_accident_grave_departement[nrow(taux_accident_grave_departement) + 1,] <- list("Aisne",NA)
taux_accident_grave_departement[nrow(taux_accident_grave_departement) + 1,] <- list("Allier",NA)
taux_accident_grave_departement[nrow(taux_accident_grave_departement) + 1,] <- list("Alpes-de-Haute-Provence",NA)
taux_accident_grave_departement[nrow(taux_accident_grave_departement) + 1,] <- list("Hautes-Alpes",NA)
taux_accident_grave_departement[nrow(taux_accident_grave_departement) + 1,] <- list("Alpes-Maritimes",NA)
taux_accident_grave_departement[nrow(taux_accident_grave_departement) + 1,] <- list("Ardèche",NA)
taux_accident_grave_departement[nrow(taux_accident_grave_departement) + 1,] <- list("Ardennes",NA)
taux_accident_grave_departement[nrow(taux_accident_grave_departement) + 1,] <- list("Ariège",NA)

#On trie dans l'ordre des noms des départements pour que les calculs après soient corrects
taux_accident_grave_departement<-taux_accident_grave_departement[order(taux_accident_grave_departement$nom_departement),]
accident_departement<-accident_departement[order(accident_departement$nom_departement),]

#On calcule les taux (il faut pour ça que dans les deux tableaux le département soient sur la même ligne, d'où le tri précédent)
taux_accident_grave_departement$n=taux_accident_grave_departement$n/accident_departement$n


carte_d2 <- geojsonio::geojson_read("data/departements.geojson", what = "sp")
carte_d2 = carte_d2[c(-14),]
carte_d2$accident<-0

carte_d2$accident<-taux_accident_grave_departement$n[match(tolower(carte_d2$nom),tolower(taux_accident_grave_departement$nom_departement))]


pal_d <- colorBin(
  "Reds", carte_d2$accident, 10
)

labels_d <- sprintf(
  "<strong>%s</strong><br/>%g",
  carte_d2$nom, carte_d2$accident
) %>% lapply(htmltools::HTML)

map4 <- leaflet(carte_d2) %>%
  setView(4,47,zoom=6)%>%
  addPolygons(
    fillColor = ~pal_d(accident),
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
      bringToFront = TRUE),
    label=labels_d)%>%
  addLegend(pal = pal_d, values = ~nom, opacity = 0.7, title = "Taux accidents graves/département",
            position = "bottomright")

mapshot(map4, file = paste0(getwd(), "/taux_accidents_graves_departement.png"))
map4

}