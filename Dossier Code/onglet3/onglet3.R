# Chargement des packages nécessaires
library(shiny); library(plotly); library(dplyr); library(tidyr)

# Chargement des données depuis le fichier CSV en ligne
JO <- read.csv("https://raw.githubusercontent.com/GabinMISARA/olympique-/ca403a2a076306fa328dfe257c889fd35e2fef4e/Dossier%20Code/athlete_events.csv", sep = ";")


# Chargement des packages nécessaires
library(shiny)
library(plotly)
library(dplyr)
library(tidyr)

# Chargement des données depuis le fichier CSV en ligne
datajo <- read.csv("athlete_events.csv", sep = ";")
datajo$Medal[is.na(datajo$Medal)] <- 0

# Partie 1: Liste des pays hôtes
hosts <- unique(datajo$Host.country)

# Partie 2: Calcul du nombre total d'athlètes par année, saison et pays hôte
athletes_count_total <- aggregate(cbind(Nb_athletes_Total = seq_along(Year)) ~ Year + Season + Host.country, data = datajo, length)

# Partie 3: Filtrer les données pour inclure uniquement les pays hôtes
datajo_hote <- datajo[datajo$Team %in% hosts, ]

# Partie 4: Calcul du nombre d'athlètes par année, saison, pays hôte
athletes_count_host <- aggregate(cbind(Nb_athletes_Host = seq_along(Year)) ~ Year + Season + Team + Host.country, data = datajo_hote, length)

# Partie 5: Fusion des comptes d'athlètes total et par pays hôte
tableau_final_hote <- merge(athletes_count_total, athletes_count_host, by = c("Year", "Season", "Host.country"), all.x = TRUE)

# Partie 6: Calcul du pourcentage de participation pour chaque pays hôte
tableau_final_hote$Pourcentage_Participation <- (tableau_final_hote$Nb_athletes_Host / tableau_final_hote$Nb_athletes_Total) * 100

# Partie 7: Compter le nombre de médailles pour chaque équipe hôte
datajo <- datajo %>%
  mutate(Medal_Gold = as.numeric(Medal == "Gold"),
         Medal_Silver = as.numeric(Medal == "Silver"),
         Medal_Bronze = as.numeric(Medal == "Bronze"))

medal_count <- datajo %>%
  group_by(Team, Year, Season, Host.country) %>%
  summarise(Medal_Gold = sum(Medal_Gold),
            Medal_Silver = sum(Medal_Silver),
            Medal_Bronze = sum(Medal_Bronze),
            total_Medals = sum(Medal_Gold, Medal_Silver, Medal_Bronze))

# Partie 8: Fusionner avec le tableau de participation
tableau_final_hote <- merge(tableau_final_hote, medal_count, by = c("Year", "Season", "Team", "Host.country"), all.x = TRUE)

# Partie 9: Arrondir les valeurs des pourcentages
tableau_final_hote$Pourcentage_Participation <- round(tableau_final_hote$Pourcentage_Participation, 2)

# Rajouter la condition pour les points violets et plus gros
tableau_final_hote$marker_color <- ifelse(tableau_final_hote$Team == tableau_final_hote$Host.country, "violet", "transparent")
tableau_final_hote$marker_size <- ifelse(tableau_final_hote$Team == tableau_final_hote$Host.country, 15, 10)

# Renommer les colonnes
names(tableau_final_hote) <- c("Year", "Season", "Team", "Host.country", "Nb_athletes_Total", "Nb_athletes_Host", "Pourcentage_Participation", "Medal_Gold", "Medal_Silver", "Medal_Bronze", "total_Medals", "marker_color", "marker_size")

# L'interface utilisateur (UI)
ui <- fluidPage(
  titlePanel("Performances aux Jeux Olympiques"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "Saison", label = "Sélectionnez la saison:", choices = c("Été" = "Summer", "Hiver" = "Winter"), selected = c("Summer", "Winter"), inline = TRUE),
      tags$br(),
      selectInput(inputId = "pays", label = "Sélectionnez un pays:", choices = unique(tableau_final_hote$Team), selected = "France")
    ),
    mainPanel(
      plotlyOutput("interactivePlot")
    )
  )
)

# Le serveur
server <- function(input, output) {
  output$interactivePlot <- renderPlotly({
    tableau_final_hote2 <- tableau_final_hote %>%
      filter(Season %in% input$Saison & Team == input$pays)
    
    plot <- plot_ly(data = tableau_final_hote2, x = ~Year) %>%
      add_trace(y = ~Medal_Gold, name = "Or", type = "scatter", mode = "markers",
                text = ~paste("Médailles d'Or :", Medal_Gold, "<br>Année :", Year, "<br> Pays :", Host.country),
                hoverinfo = "text", marker = list(color = "gold", size = tableau_final_hote2$marker_size, line = list(color = tableau_final_hote2$marker_color))) %>%
      add_trace(y = ~Medal_Silver, name = "Argent", type = "scatter", mode = "markers",
                text = ~paste("Médailles d'Argent :", Medal_Silver, "<br>Année :", Year, "<br> Pays :", Host.country),
                hoverinfo = "text", marker = list(color = "silver", size = tableau_final_hote2$marker_size, line = list(color = tableau_final_hote2$marker_color))) %>%
      add_trace(y = ~Medal_Bronze, name = "Bronze", type = "scatter", mode = "markers",
                text = ~paste("Médailles de Bronze :", Medal_Bronze, "<br>Année :", Year, "<br> Pays :", Host.country),
                hoverinfo = "text", marker = list(color = "brown", size = tableau_final_hote2$marker_size, line = list(color = tableau_final_hote2$marker_color))) %>%
      add_trace(y = ~Pourcentage_Participation, name = "Pourcentage Participation", type = "scatter", mode = "markers",
                yaxis = "y2",
                text = ~paste("Pourcentage de Participation :", Pourcentage_Participation, "%<br>Année :", Year, "<br> Pays :", Host.country),
                hoverinfo = "text", marker = list(color = "blue", size = tableau_final_hote2$marker_size)) %>%
      layout(title = paste("Performances aux Jeux Olympiques", input$pays, ":"),
             xaxis = list(title = "Années"),
             yaxis = list(title = "Nombre de Médailles", side = "left", showgrid = FALSE),
             yaxis2 = list(title = "Pourcentage Participation", side = "right", overlaying = "y", showgrid = FALSE),
             showlegend = TRUE)
    
    plot
  })
}

# Exécution de l'application Shiny
shinyApp(ui = ui, server = server)



####### Test Adrien #######

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
    
    # datajo <- read.csv("https://raw.githubusercontent.com/GabinMISARA/olympique-/ca403a2a076306fa328dfe257c889fd35e2fef4e/Dossier%20Code/athlete_events.csv", sep = ";")
    datajo <- read.csv("/Users/adrienromand/Documents/OPEN/olympique-/Dossier Code/athlete_events.csv")
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
  })
  
  p
}

##### Pistes amélioration 
#### Afficher lieu + nb diff médailles quand l'utilisateur passe sur les barres avec la souris
#### Thème graphique
#### Arrière-plan
#### METTRE LES BONNES DONNÉES
#### Remplacer le nom des pays en français dans le titre du ggplot
#### Création de l'histogramme avec Plotly
#### À la place des couleurs, remplir les barres en 3 parties représentant le % de chaque médaille




