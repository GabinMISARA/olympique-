# Charger les bibliothèques nécessaires
library(shiny)
library(dplyr)
library(plotly)

# Spécifier l'URL du fichier CSV sur Kaggle
url <- "https://raw.githubusercontent.com/GabinMISARA/olympique-/ca403a2a076306fa328dfe257c889fd35e2fef4e/Dossier%20Code/athlete_events.csv"

# Lire le fichier CSV depuis l'URL
olympics_data <- read.csv(url, sep = ";")

# Obtenir la première année où le jeu a été inclus aux JO
first_year <- min(olympics_data$Year, na.rm = TRUE)

# Liste des pays uniques
unique_countries <- unique(olympics_data$NOC)

# Définir l'interface utilisateur (UI)
ui <- fluidPage(
  titlePanel("Onglet 1 - Graph 1 : Global"),
  titlePanel("Visualisation des résultats aux JO"),
  
  # Onglets
  tabsetPanel(
    tabPanel("Graph 1 : Global",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("annee_slider", "Sélectionnez une année :", 
                             min = first_year, 
                             max = max(olympics_data$Year, na.rm = TRUE),
                             value = c(first_year, max(olympics_data$Year, na.rm = TRUE))),
                 selectInput("sport_select", "Sélectionnez un sport :", 
                             choices = c("Tous", unique(olympics_data$Sport))),
                 selectInput("country_select", "Sélectionnez un pays :",
                             choices = c("Tous", unique_countries)),
                 width = 3
               ),
               mainPanel(
                 plotlyOutput("graphique_global")
               )
             )
    )
  )
)

# Définir le serveur
server <- function(input, output) {
  
  # Filtrer les données en fonction des sélections
  data_filtered <- reactive({
    filtered_data <- olympics_data
    if (input$sport_select != "Tous") {
      filtered_data <- filter(filtered_data, Sport == input$sport_select)
    }
    if (input$country_select != "Tous") {
      filtered_data <- filter(filtered_data, NOC == input$country_select)
    }
    if (!is.null(input$annee_slider)) {
      filtered_data <- filter(filtered_data, Year >= input$annee_slider[1] & Year <= input$annee_slider[2])
    }
    # Exclure les lignes où Medal est NA
    filtered_data <- filter(filtered_data, !is.na(Medal))
    filtered_data
  })
  
  # Créer le graphique global en utilisant plotly
  output$graphique_global <- renderPlotly({
    plot_ly(data_filtered(), x = ~Year, color = ~Medal, type = "histogram", 
      colors = c("Gold" = "gold","Silver" = "grey","Bronze" = "darkorange")) %>%
      layout(title = "Résultats aux JO",
             xaxis = list(title = "Année"),
             yaxis = list(title = "Nombre de médailles"),
             barmode = "stack",
             showlegend = TRUE,
             legend = list(title = "Médaille"))
             
  })
}

# Créer l'application Shiny
shinyApp(ui, server)

