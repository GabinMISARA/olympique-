# Charger les bibliothèques nécessaires
library(shiny); library(ggplot2); library(dplyr)

# Spécifier l'URL du fichier CSV sur Kaggle
JO <- read.csv("https://raw.githubusercontent.com/GabinMISARA/olympique-/ca403a2a076306fa328dfe257c889fd35e2fef4e/Dossier%20Code/athlete_events.csv", sep = ";")
# Obtenir la première année où le jeu a été inclus aux JO
first_year <- min(JO$Year, na.rm = TRUE)
# Liste des pays uniques
unique_countries <- unique(JO$NOC)
# Définir l'interface utilisateur (UI)
ui <- fluidPage(
  
  #logo
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('https://upload.wikimedia.org/wikipedia/commons/thumb/a/a7/Olympic_flag.svg/2560px-Olympic_flag.svg.png'); /* Remplacez l'URL par l'URL de votre image */
        background-size: 100px;
        background-position:right top;
        background-repeat: no-repeat;
      }
    "))
  ),
  
  titlePanel("Onglet 1 - Graph 1 : Global"),
  titlePanel("Histogramme des résultats aux JO"),
  
  # Onglets
  tabsetPanel(
    tabPanel("Graph 1 : Global",
             sidebarLayout(
               sidebarPanel(
                 tags$style(".well {background-color:#CFB095;}"), #change la couleur du fond
                 sliderInput("annee_slider", "Sélectionnez une année :", 
                             min = first_year, 
                             max = max(JO$Year, na.rm = TRUE),
                             value = c(first_year, max(JO$Year, na.rm = TRUE))),
                 selectInput("sport_select", "Sélectionnez un sport :", 
                             choices = c("Tous", unique(JO$Sport))),
                 selectInput("country_select", "Sélectionnez un pays :",
                             choices = c("Tous", unique_countries)),
                 width = 3
               ),
               mainPanel(
                 plotOutput("graphique_global")
               )
             )
    )
  )
)

# Définir le serveur
server <- function(input, output) {
  
  # Filtrer les données en fonction des sélections
  data_filtered <- reactive({
    filtered_data <- JO
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
  
  # Créer le graphique global
  output$graphique_global <- renderPlot({
    ggplot(data_filtered(), aes(x = Year, fill = Medal)) +
      geom_bar(stat = "count") +
      labs(title = "Résultats aux JO",
           x = "Année",
           y = "Nombre de médailles",
           fill = "Médaille") +
      scale_fill_manual(values = c("Gold" = "gold", "Silver" = "grey", "Bronze" = "darkorange")) +
      theme_minimal()
  })
}

# Créer l'application Shiny
shinyApp(ui, server)