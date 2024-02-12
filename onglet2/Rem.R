#Onglet 2

# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("plotly")
# install.packages("dplyr")

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# On récupère les données
JO <- read.csv("https://raw.githubusercontent.com/GabinMISARA/olympique-/main/athlete_events.csv", sep = ";")

# On filtre les NA
JO_filt <- JO[complete.cases(JO$Medal), ]


# On définit l'interface utilisateur
ui <- fluidPage(
  # Titre de l'application
  titlePanel("Athlètes"),
  
  # Panneau latéral
  sidebarLayout(
    sidebarPanel(
      # Case à cocher
      checkboxInput(inputId = "Summer", label = "Summer", value = TRUE),
      checkboxInput(inputId = "Winter", label = "Winter", value = TRUE),
      tags$br(),  # Saut de ligne
      # Liste déroulante pour sélectionner le rapport à choisir
      selectInput(inputId = "choix", label = "Sélectionnez les données à visualiser: ",
                  selected = "Homme & Femme",
                  choices = c("Homme & Femme", "Homme uniquement", "Femme uniquement")),
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# On définit le serveur
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    # Filtrage des données en fonction des choix de l'utilisateur
    if (!input$Summer) {
      # Filtre pour la saison été
      JO_filt <- JO_filt[JO_filt$Season != "Summer", ]
    }
    
    if (!input$Winter) {
      # Filtre pour la saison hiver
      JO_filt <- JO_filt[JO_filt$Season != "Winter", ]
    }
    
    # Affiche les données filtrées pour déboguer
    print(JO_filt)
    
    # Création du graphique en fonction de la sélection
    if (input$choix == "Homme & Femme") {
      plot_ly(data = JO_filt %>% distinct(NOC, Sex, .keep_all = TRUE),
              x = ~NOC, type = 'bar', color = ~Sex) %>%
        layout(title = 'Performances aux Jeux Olympiques par Pays',
               xaxis = list(title = 'Pays'),
               yaxis = list(title = 'Nombre de Médailles'))
    } else if (input$choix == "Homme uniquement") {
      JO_homme <- JO_filt %>% filter(Sex == "M") %>% distinct(NOC, .keep_all = TRUE)
      plot_ly(data = JO_homme, x = ~NOC, type = 'bar', color = ~Medal) %>%
        layout(title = 'Performances aux Jeux Olympiques par Pays (Hommes uniquement)',
               xaxis = list(title = 'Pays'),
               yaxis = list(title = 'Nombre de Médailles'))
    } else if (input$choix == "Femme uniquement") {
      JO_femme <- JO_filt %>% filter(Sex == "F") %>% distinct(NOC, .keep_all = TRUE)
      plot_ly(data = JO_femme, x = ~NOC, type = 'bar', color = ~Medal) %>%
        layout(title = 'Performances aux Jeux Olympiques par Pays (Femmes uniquement)',
               xaxis = list(title = 'Pays'),
               yaxis = list(title = 'Nombre de Médailles'))
    }
  })
}

# Exécution de l'application Shiny
shinyApp(ui, server)