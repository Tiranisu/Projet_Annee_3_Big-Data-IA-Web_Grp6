install.packages("leaflet")
install.packages("geojsonio")
install.packages("htmltools")
library(leaflet)
library(dplyr)
library(geojsonio)
library(htmltools)

#https://rstudio.github.io/leaflet/choropleths.html

visualisation_carte_region <- function() {
    carte_r <- geojsonio::geojson_read("data/regions.geojson", what = "sp")
    carte_r = carte_r[c(-14),]
    carte_r$accident<-0

    carte_r$accident<-accident_region$n[match(tolower(accident_region$nom_region),tolower(carte_r$nom))]


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

    # mapshot(map1, file = paste0(getwd(), "/accident_region.png"))
}


visualisation_carte_departement <- function() {
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

    # mapshot(map2, file = paste0(getwd(), "/accident_departement.png"))
}