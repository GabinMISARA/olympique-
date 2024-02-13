### Onglet 2
# Chargement des packages nécessaires
library(shiny); library(plotly); library(dplyr); library(tidyr); library(gtranslate)

# Chargement des données depuis notre fichier CSV en ligne
JO <- read.csv("https://raw.githubusercontent.com/GabinMISARA/olympique-/ca403a2a076306fa328dfe257c889fd35e2fef4e/Dossier%20Code/athlete_events.csv", sep = ";")

# Filtre les lignes avec des valeurs manquantes dans la colonne Medal
JO_filt <- JO[complete.cases(JO$Medal), ]

# On définit l'interface utilisateur
ui <- fluidPage(
  # Titre de l'application
  titlePanel("Athlètes"),
  
  # Mise en page avec un panneau latéral et un panneau principal
  sidebarLayout(
    sidebarPanel(
      # Cases à cocher pour sélectionner la saison (été/hiver)
      checkboxGroupInput(inputId = "Saison", label = "Sélectionnez la saison:",
                         choices = c("Été" = "Summer", "Hiver" = "Winter"),
                         selected = c("Summer", "Winter"), inline = TRUE),
      # Cases à cocher pour sélectionner le genre (homme/femme)
      checkboxGroupInput(inputId = "Genre", label = "Sélectionnez le genre:",
                         choices = c("Homme"="M", "Femme"="F"),
                         selected = c("M", "F"), inline = TRUE),
      tags$br(),  # Saut de ligne
      # Liste déroulante pour sélectionner un pays
      selectInput(inputId = "pays", label = "Sélectionnez un pays:", 
                  choices = unique(JO_filt$NOC),
                  selected = "FRA")
      ),
    mainPanel(
      plotlyOutput("interactivePlot")
    )
  )
)

# On définit le serveur
server <- function(input, output) {
  # Définition de la sortie interactive dans Shiny
  output$interactivePlot <- renderPlotly({
    
    # Filtre les données en fonction de la sélection faites par l'utilisateur
    JO_filt2 <- JO_filt %>%
      filter(Season %in% input$Saison & 
               Sex %in% input$Genre & 
               NOC == input$pays)
    
    # Sélection des colonnes Year/Année, Medal/Médaille et City/Ville
    JO_filt2 <- JO_filt2 %>% select(Year, Medal, City)
    
    # Agrégation des données par année avec le nombre de médailles de chaque type
    cmed <- JO_filt2 %>%
      group_by(Year) %>%
      summarize(Bronze = sum(Medal == "Bronze"),
                Argent = sum(Medal == "Silver"),
                Or = sum(Medal == "Gold"),
                City = first(City))
    
    # Transformation des données en format long pour une utilisation facile dans Plotly
    cmedailles <- pivot_longer(cmed, cols = c(Bronze, Argent, Or),
                               names_to = "Medal", values_to = "Count")
    
    # Fonction pour traduire le nom du pays (NOC) en français avec une gestion des erreurs
    NPE <- function(NOC) {
      traduction <- tryCatch(
        {
          translate(paste("for the", unique(JO_filt$Team[JO_filt$NOC == NOC])), to = "fr")
        },
        error = function(e) {NOC})
      return(traduction)
    } 
    
    # Création du graphique interactif avec Plotly
    plot <- plot_ly(data = cmedailles, x = ~Year, y = ~Count, color = ~Medal,
                    type = "scatter", mode = "lines+markers", line = list(shape = "spline", smoothing = 0.65),
                    text = ~paste(Medal, ":", Count, " médailles", "<br>Année :", Year, "<br> Ville :", City),
                    hoverinfo = "text",
                    colors = c("Bronze" = "darkgoldenrod", "Argent" = "grey", "Or" = "gold")) %>%
      layout(title = paste("Performances aux Jeux Olympiques", NPE(input$pays), ":"),
             xaxis = list(title = "Années"),
             yaxis = list(title = "Nombre de Médailles"),
             showlegend = TRUE,
             margin = list(t = 100),
             height = 600,
             legend = list(orientation = "h", entrywidth = 70, yanchor = "bottom", y = 1.02, xanchor = "right", x = 1)
      ) %>%
      config(displayModeBar = TRUE)  # Activer la barre d'options interactive
    # Affiche le graphique
    print(plot)
  })
}

# Lancement de l'application Shiny avec l'interface utilisateur (ui) et le serveur (server) définis précédemment
shinyApp(ui, server)