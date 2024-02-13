# Onglet 2

# install.packages("shiny")
# install.packages("plotly")
# install.packages("dplyr")
# install.packages("tidyr")

library(shiny)
library(plotly)
library(dplyr)
library(tidyr)

# On récupère les données
JO <- read.csv("C:/Users/remli/Documents/Perso/Cours/4A/OPEN/rendu groupe/athlete_events.csv")
# JO <- read.csv("https://raw.githubusercontent.com/GabinMISARA/olympique-/main/athlete_events.csv", sep = ";")

# On filtre les NA
JO_filt <- JO[complete.cases(JO$Medal), ]

# On définit l'interface utilisateur
ui <- fluidPage(
  # Titre de l'application
  titlePanel("Athlètes"),
  
  
  # Panneau latéral
  sidebarLayout(
    sidebarPanel(
      # Groupes de cases à cocher pour Summer et Winter
      checkboxGroupInput(inputId = "Saison", label = "Sélectionnez la saison:",
                         choices = c("Été" = "Summer", "Hiver" = "Winter"),
                         selected = c("Summer", "Winter"), inline = TRUE),
      # Groupes de cases à cocher pour Homme et Femme
      checkboxGroupInput(inputId = "Genre", label = "Sélectionnez le genre:",
                         choices = c("Homme"="M", "Femme"="F"),
                         selected = c("M", "F"), inline = TRUE),
      tags$br(),  # Saut de ligne
      # Liste déroulante pour sélectionner un pays
      selectInput(inputId = "pays", label = "Sélectionnez un pays:", 
                  choices = unique(JO_filt$NOC),
                  selected = "FRA"),
    ),
    mainPanel(
      plotlyOutput("interactivePlot")
    )
  )
)

# On définit le serveur
server <- function(input, output) {
  output$interactivePlot <- renderPlotly({
    # Filtrage des données en fonction des choix de l'utilisateur
    JO_filt2 <- JO_filt %>%
      filter(Season %in% input$Saison &
               Sex %in% input$Genre &
               NOC == input$pays)
    JO_filt2 <- JO_filt2 %>%
      select(Year, Medal, City, Team) #On ajoute la colonne City & Team

    
    # Calcul du nombre total de médailles pour chaque année
    cmed<- JO_filt2 %>%
      group_by(Year) %>%
      summarize(Bronze = sum(Medal == "Bronze"),
                Silver = sum(Medal == "Silver"),
                Gold = sum(Medal == "Gold"),
                City = first(City))  # Assuming City is the same for a given year
    
    # Transformation des données pour le format long
    cmedailles <- pivot_longer(cmed, cols = c(Bronze, Silver, Gold),
                                      names_to = "Medal", values_to = "Count")
    
    # Création du graphique en fonction de la sélection
    plot <- plot_ly(data = cmedailles, x = ~Year, y = ~Count, color = ~Medal,
                    type = "scatter", mode = "lines+markers", line = list(shape = "spline", smoothing = 1),
                    text = ~paste(Medal, ":", Count, " médailles", "<br>Année :", Year, "<br> Ville :", City),
                    hoverinfo = "text+name",
                    colors = c("Bronze" = "darkgoldenrod", "Silver" = "grey", "Gold" = "gold"),
                    legendgroup = ~Medal) %>%
      layout(title = paste("Performances aux Jeux Olympiques pour,", input$pays, ":"),
             xaxis = list(title = "Années"),
             yaxis = list(title = "Nombre de Médailles"),
             showlegend = TRUE,
             margin = list(t = 100),
             height = 600,
             legend = list(orientation = "h", entrywidth = 70, yanchor = "bottom", y = 1.02, xanchor = "right", x = 1)) %>%
      config(displayModeBar = TRUE)  # Activer la barre d'options interactive
    
    # Affichage du graphique
    print(plot)
  })
}

# Exécution de l'application Shiny
shinyApp(ui, server)