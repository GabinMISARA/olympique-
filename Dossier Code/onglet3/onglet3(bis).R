# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
# git # https://github.com/GabinMISARA/olympique-

library(shiny)
library(ggplot2)
#install.packages("shinyWidgets")
#install.packages("plotly")
library(shinyWidgets)
library(plotly)
  
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
        choices = c("", hote_alphab),
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
    
    datajo <- read.csv("https://raw.githubusercontent.com/GabinMISARA/olympique-/ca403a2a076306fa328dfe257c889fd35e2fef4e/Dossier%20Code/athlete_events.csv", sep = ";")
    # Liste des pays hôtes
    hote <- unique(datajo$Host.country)
    # Compter le nombre de médailles pour chaque équipe
    
    medal_count <- aggregate(Medal ~ Team + Year, data=datajo, function(x) 
      table(factor(x, levels=c("Gold", "Silver", "Bronze"))))
    
    datajo2 <- datajo[, -c(17, 18, 19)] 
    hote_alphab <- sort(hote)
    
    # Filtrer les données pour ne conserver que les lignes où le nom de l'équipe correspond au pays sélectionné
    datajo_hote <- datajo2[datajo2$Team == input$pays, ]
    # Remplacer les NA par 0
    datajo_hote$Medal[is.na(datajo_hote$Medal)] <- 0
    
    # Compter le nombre total de médailles remportées par année
    medal_count2 <- aggregate(Medal ~ Year, data = datajo_hote, FUN = function(x) length(na.omit(x)))
    
    # Définir l'année ou les années où le pays selctionné a été hôte des JO
    #annee_hote <- unique(datajo_hote$Year)
    
    # Identifier les saisons (été ou hiver) des Jeux Olympiques
   # saison <- unique(datajo_hote$Season)
    
    # Créer l'histogramme
    barplot(medal_count2$Medal, names.arg = medal_count2$Year, 
            main = paste("Nombre total de médailles pour", input$pays),
            xlab = "Année", ylab = "Nombre de médailles remportées",
            col = ifelse(medal_count2$Year %in% datajo_hote$Year & datajo_hote$Season == "Summer", adjustcolor("red", 0.5), 
                         ifelse(medal_count2$Year %in% datajo_hote$Year & datajo_hote$Season == "Winter", adjustcolor("blue", 0.5), 
                                ifelse(datajo_hote$Season == "Summer", adjustcolor("red", 0.9), adjustcolor("blue", 0.9)))),  # Rouge clair pour l'été, bleu ciel pour l'hiver, rouge foncé pour l'hiver, bleu foncé pour l'été
    border = ifelse(medal_count2$Year %in% datajo_hote$Year, "yellow", "white"),  # Contour jaune pour les années hôtes
    space = 0.5
  )})
}



shinyApp(ui = ui, server = server)

##### Pistes amélioration 
#### METTRE LES BONNES DONNÉES
#### Remplacer le nom des pays en français dans le titre du ggplot
#### Création de l'histogramme avec Plotly
#### À la place des couleurs, remplir les barres en 3 parties représentant le % de chaque médaille

# Création de l'histogramme avec Plotly
p <- plot_ly(
  x = ~medal_count2$Year,
  type = 'bar',
  hoverinfo = 'text',
  text = paste("Année: ", medal_count2$Year,
               "<br>Médailles d'or: ", medal_count2$Gold,
               "<br>Médailles d'argent: ", medal_count2$Silver,
               "<br>Médailles de bronze: ", medal_count2$Bronze,
               "<br>Pays hôte: ", input$pays,
               "<br>Ville: ", datajo_hote$City)
)

# Ajouter la trace pour les médailles d'or
p <- p %>% add_trace(
  y = ~medal_count2$Gold,
  name = 'Or',
  marker = list(color = 'gold', line = list(color = ifelse(medal_count2$Year %in% annees_hote, 'black', 'white'), width = 1)),
  hoverinfo = 'none'
)

# Ajouter la trace pour les médailles d'argent
p <- p %>% add_trace(
  y = ~medal_count2$Silver,
  name = 'Argent',
  marker = list(color = 'silver', line = list(color = ifelse(medal_count2$Year %in% annees_hote, 'black', 'white'), width = 1)),
  hoverinfo = 'none'
)

# Ajouter la trace pour les médailles de bronze
p <- p %>% add_trace(
  y = ~medal_count2$Bronze,
  name = 'Bronze',
  marker = list(color = 'peru', line = list(color = ifelse(medal_count2$Year %in% annees_hote, 'black', 'white'), width = 1)),
  hoverinfo = 'none'
)

p <- p %>%
  layout(
    title = paste("Nombre total de médailles pour", input$pays),
    xaxis = list(title = "Année"),
    yaxis = list(title = "Nombre de médailles"),
    barmode = 'stack'  # Pour empiler les barres
  )

p
})

#### Afficher lieu + nb diff médailles quand l'utilisateur passe sur les barres avec la souris
#### Thème graphique
#### Arrière-plan


