git # https://github.com/GabinMISARA/olympique-
##### Partie ADRIEN ######
# Liste des pays hôtes
hote <- datajo$ "NOC = Pays"

# Résultats historiques rapportés au nombre d'athlètes du pays lui même 
#rapporté au nombre total d'athlètes participants
  
exte <- datajo$ "NOC ≠ Pays"

bronze <- as.numeric(exte$Medal = "Bronze")
argent <- as.numeric(exte$Medal = "Sliver")
or <- as.numeric(exte$Medal = "Gold")

res_3 <- bronze*("nb athlètes/nb tot")
res_2 <- argent*("nb athlètes/nb tot")
res_1 <- or*("nb athlètes/nb tot")

res_tot <- res_1 + res_2 + res_3

# Moyenne des résultats historiques

res_moy <- res_tot/"nb année"

# Résultats quand hôte rapportés au nombre d'athlètes du pays lui même 
#rapporté au nombre total d'athlètes participants
  
h_bronze <- as.numeric(hote$Medal = "Bronze")
h_argent <- as.numeric(hote$Medal = "Silver")
h_or <- as.numeric(hote$Medal = "Gold")

res_h3 <- h_bronze*("nb athlètes/nb tot")
res_h2 <- h_argent*("nb athlètes/nb tot")
res_h1 <- h_or*("nb athlètes/nb tot")

res_h_tot <- res_h1 + res_h2 + res_h3

# Moyenne des résultats quand hôte

res_h_moy <- res_h/"nb année"

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

###### PARTIE GABIN ########
library(shiny)
library(shinyWidgets)
library(ggplot2)

ui <- fluidPage(
  
  # Titre de l'application
  titlePanel("Sélection de Pays"),
  
  # Sélection de pays
  sidebarLayout(
    sidebarPanel(
      # Widget de sélection de pays
      pickerInput(
        inputId = "pays",
        label = "Sélectionnez un pays :",
        choices = c("","France", "Germany", "United Kingdom", "United States"), #remplacer par hote
        selected = NULL,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 2",
          liveSearch = TRUE,
          noneSelectedText = NULL
        )
      )
    ),
    mainPanel(
      # Affichage des informations sélectionnées
      textOutput("pays_output"),
      plotOutput("hist")
    )
  )
)

server <- function(input, output) {
  # Renvoie le pays sélectionné
  output$pays_output <- renderText({
    if (input$pays == "") {
      "Aucun pays sélectionné"
    } else {
    }
  })
  
  output$hist <- renderPlot({
    req(input$pays)
    pays_data <- data[datajo$Pays == input$pays, ]
    ggplot(pays_data, aes(x = factor(Year), y = nb_medailles,fill = factor(pays_accueil))) +
      geom_bar(stat = "") +
      scale_fill_manual(values = c("red", "black")) +
      labs(title = paste("Nombre de Médailles pour", input$selected_country, 
                         "par Année"),
           x = "Année", y = "Nombre de Médailles") +
      theme_minimal()
  })
}

   

shinyApp(ui = ui, server = server)
