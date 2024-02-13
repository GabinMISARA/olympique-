library(shiny)
library(leaflet)

# Données des pays avec leurs coordonnées géographiques
pays <- data.frame(
  nom = c("Norvège", "Suède", "Finlande", "Grande Bretagne", "Pays Bas", "France", "Espagne", "Italie", 
          "Autriche", "Belgique", "Danemark", "Turquie", "Hongrie", "Pologne", "Roumanie", "Canada", 
          "Etats-Unis", "Australie", "Chine", "Japon", "Japon", "Russie", "Brésil", "Argentine"),
  latitude = c(60.472024, 60.128161, 61.92411, 51.509865, 52.132633, 46.603354, 40.463667, 41.902782, 47.516231, 
               50.503887, 56.26392, 38.963745, 47.162494, 51.919438, 45.943161, 56.130366, 37.09024, 
               -25.274398, 36.204824, 36.204824, 36.204824, 61.52401, -14.235004, -38.416097),
  longitude = c(8.468946, 18.643501, 25.748151, -0.118092, 5.291266, 1.888334, -3.74922, 12.496366, 14.550072, 
                4.469936, 9.501785, 35.243322, 19.503304, 19.145136, 24.96676, -106.346771, -106.346771, 
                133.775136, 104.195397, 138.252924, 138.252924, 138.252924, -51.92528, -63.616672),
  medaille_or = c(192, 205,144, 296, 130, 262, 49, 257, 83, 44, 48, 41, 182, 79, 90, 144, 1175, 169, 275, 183, 183, 195, 37, 21 )
)

ui <- fluidPage(
  titlePanel("Application Shiny pour la visualisation des vainqueurs aux JO"),
  tabsetPanel(
    tabPanel("Nombre de médailles d'or gagné ", 
             leafletOutput("map", width = "100%", height = "600px")
    ),
    tabPanel("Onglet 2", "Contenu de l'onglet 2"),
    tabPanel("Onglet 3", "Contenu de l'onglet 3"),
    tabPanel("A la maison on est champion ? ", "Contenu de l'onglet 4")
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addCircleMarkers(
        data = pays,
        lng = ~longitude, lat = ~latitude,
        radius = sqrt(pays$medaille_or),
        color = "blue",
        fillColor = "red",
        fillOpacity = 0.8,
        popup = ~paste("<b>", nom, "</b><br>",
                       "Médailles d'or: ", medaille_or)
      )
  })
}

shinyApp(ui = ui, server = server)
