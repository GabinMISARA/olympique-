
# Chargement des packages nécessaires

library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinyWidgets)

# Chargement des données depuis le fichier CSV en ligne
datajo <- read.csv ("https://raw.githubusercontent.com/GabinMISARA/olympique-/main/Dossier%20Code/athlete_events.csv", sep = ";")

###### Définition des variables Graphique 1 #####
#datajo <- JO
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

# Partie 8: Fusionner avec le tableau de participation bb
tableau_final_hote <- merge(tableau_final_hote, medal_count, by = c("Year", "Season", "Team", "Host.country"), all.x = TRUE)

# Partie 9: Arrondir les valeurs des pourcentages
tableau_final_hote$Pourcentage_Participation <- round(tableau_final_hote$Pourcentage_Participation, 2)

# Rajouter la condition pour les points violets et plus gros
tableau_final_hote$marker_color <- ifelse(tableau_final_hote$Team == tableau_final_hote$Host.country, "white", "transparent")
tableau_final_hote$marker_size <- ifelse(tableau_final_hote$Team == tableau_final_hote$Host.country, 15, 10)

# Renommer les colonnes
names(tableau_final_hote) <- c("Year", "Season", "Team", "Host.country", "Nb_athletes_Total", "Nb_athletes_Host", "Pourcentage_Participation", "Medal_Gold", "Medal_Silver", "Medal_Bronze", "total_Medals", "marker_color", "marker_size")



###### Définition des variables Graphique 2 #####
hote <- unique(datajo$Host.country)
# Compter le nombre de médailles pour chaque équipe

medal_count <- aggregate(Medal ~ Team + Year, data=datajo, function(x) 
  table(factor(x, levels=c("Gold", "Silver", "Bronze"))))


datajo2 <- datajo[, -c(17, 18, 19)] 
hote_alphab <- sort(hote)

# Sélectionner les lignes de datajo2 où la valeur de la colonne "Team" est dans hote_alphab
datajo_hote <- datajo2[datajo2$Team %in% hote_alphab, ]

# Compter le nombre total de médailles remportées par année
datajo_hote <- datajo_hote %>%
  mutate(Medal_Gold = as.numeric(Medal == "Gold"),
         Medal_Silver = as.numeric(Medal == "Silver"),
         Medal_Bronze = as.numeric(Medal == "Bronze"))
medal_count2 <- datajo_hote %>%
  group_by(Team, Year, Season, Host.country) %>%
  summarise(
    Medal_Gold = sum(Medal_Gold, na.rm = TRUE),
    Medal_Silver = sum(Medal_Silver, na.rm = TRUE),
    Medal_Bronze = sum(Medal_Bronze, na.rm = TRUE),
    total_Medals = sum(Medal_Gold, Medal_Silver, Medal_Bronze, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  replace_na(list(Medal_Gold = 0, Medal_Silver = 0, Medal_Bronze = 0, total_Medals = 0))


###### Définition des variables Graphique 3 #####
datajo3 <- datajo %>%
  mutate(Medal_Gold = as.numeric(Medal == "Gold"),
         Medal_Silver = as.numeric(Medal == "Silver"),
         Medal_Bronze = as.numeric(Medal == "Bronze")) %>%
  group_by(Team, Year) %>%
  summarise(
    total_Medals = sum(Medal_Gold, Medal_Silver, Medal_Bronze, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  replace_na(list(Medal_Gold = 0, Medal_Silver = 0, Medal_Bronze = 0, total_Medals = 0)) %>%
  filter(Team %in% (filter(., Year >= 2010) %>% group_by(Team) %>% filter(sum(total_Medals) > 0) %>% pull(Team)))

# Exclure la Russie (en raison de la situation militaire et géo-politique actuelle)
datajo3 <- datajo3 %>%
  filter(Team != "Russia")

# Trier les données en fonction du nombre total de médailles
top_countries <- datajo3 %>%
  group_by(Team) %>%
  summarise(total_Medals = sum(total_Medals)) %>%
  top_n(10, total_Medals) %>%
  pull(Team)

# Filtrer les données pour inclure uniquement les 10 premiers pays
medal_count_top <- datajo3 %>%
  filter(Team %in% top_countries)


##### UI #####
ui <- fluidPage(
  titlePanel("Performances aux Jeux Olympiques"),
  
  # Onglets pour chaque graphique
  tabsetPanel(
    # Onglet pour le premier graphique
    tabPanel("A la maison on est champion ?", 
             sidebarLayout(
               sidebarPanel(
                 tags$h4("Paramètres de filtrage"),
                 tags$br(),   # Saut de ligne
                 checkboxGroupInput(inputId = "Saison2", label = "Sélectionnez la saison (graphique 1):", choices = c("Été" = "Summer", "Hiver" = "Winter"), selected = c("Summer", "Winter"), inline = TRUE),
                 tags$br(),
                 selectInput(inputId = "pays2", label = "Sélectionnez un pays:", choices = unique(tableau_final_hote$Team), selected = "Australia"),
                 pickerInput(
                   inputId = "pays",
                   label = "Sélectionnez un pays (graphique 2) :",
                   choices = c("", hote_alphab),
                   selected = NULL,
                   options = list(
                     `actions-box` = TRUE,
                     `selected-text-format` = "count > 2",
                     liveSearch = TRUE,
                     noneSelectedText = NULL
                   )
                 ),
                 width = 3
               ),
               mainPanel(
                 fluidRow(
                   column(width = 12, style = "padding: 20px;", div(plotlyOutput("interactivePlot2"), style = "border: 2px solid #ccc; border-radius: 5px; margin-bottom: 20px;")),
                   column(width = 12, style = "padding: 20px;", div(plotlyOutput("hist"), style = "border: 2px solid #ccc; border-radius: 5px; margin-bottom: 20px;"), div(textOutput("pays_output"))),
                   column(width = 12, style = "padding: 20px;", div(plotlyOutput("medal_trend_plot"), style = "border: 2px solid #ccc; border-radius: 5px; margin-bottom: 20px;"))
                 ),
               )
             )
    )
  )
)


###### Serveur #####
server <- function(input, output) {
  # Serveur Graphique 1
  output$interactivePlot2 <- renderPlotly({
    tableau_final_hote2 <- tableau_final_hote %>%
      filter(Season %in% input$Saison2 & Team == input$pays2)
    
    plot2 <- plot_ly(data = tableau_final_hote2, x = ~Year) %>%
      add_trace(y = ~Medal_Gold, name = "Or", type = "scatter", mode = "markers",
                text = ~paste("Médailles d'Or :", Medal_Gold, "<br>Année :", Year, "<br> Pays hôte :", Host.country, 
                              ifelse(tableau_final_hote2$marker_size == 15, "<br>Le pays sélectioné est le pays hôte", "")),
                hoverinfo = "text", marker = list(color = "gold", size = tableau_final_hote2$marker_size, line = list(color = tableau_final_hote2$marker_color))) %>%
      add_trace(y = ~Medal_Silver, name = "Argent", type = "scatter", mode = "markers",
                text = ~paste("Médailles d'Argent :", Medal_Silver, "<br>Année :", Year, "<br> Pays hôte :", Host.country, 
                              ifelse(tableau_final_hote2$marker_size == 15, "<br>Le pays sélectioné est le pays hôte", "")),
                hoverinfo = "text", marker = list(color = "silver", size = tableau_final_hote2$marker_size, line = list(color = tableau_final_hote2$marker_color))) %>%
      add_trace(y = ~Medal_Bronze, name = "Bronze", type = "scatter", mode = "markers",
                text = ~paste("Médailles de Bronze :", Medal_Bronze, "<br>Année :", Year, "<br> Pays :", Host.country, 
                              ifelse(tableau_final_hote2$marker_size == 15, "<br> Le pays sélectioné est le pays hôte", "")),
                hoverinfo = "text", marker = list(color = "brown", size = tableau_final_hote2$marker_size, line = list(color = tableau_final_hote2$marker_color))) %>%
      add_trace(y = ~Pourcentage_Participation, name = "Pourcentage Participation", type = "scatter", mode = "markers",
                yaxis = "y2",
                text = ~paste("Pourcentage de Participation :", Pourcentage_Participation, "%<br>Année :", Year, "<br> Pays hôte :", Host.country, 
                              ifelse(tableau_final_hote2$marker_size == 15, "<br> Le pays sélectioné est le pays hôte", "")),
                hoverinfo = "text", marker = list(color = "blue", size = tableau_final_hote2$marker_size)) %>%
      layout(title = paste("Performances aux Jeux Olympiques", input$pays, ":"),
             xaxis = list(title = "Années"),
             yaxis = list(title = "Nombre de Médailles", side = "left", showgrid = FALSE),
             yaxis2 = list(title = "Pourcentage Participation", side = "right", overlaying = "y", showgrid = FALSE),
             showlegend = TRUE)
    
    return(plot2)
  })
  
  # Serveur Graphique 2
  output$pays_output <- renderText({
    if (input$pays == "") {
      "Aucun pays sélectionné"
    } else {
    }
  })
  
  output$hist <- renderPlotly({
    req(input$pays)
    
    if (!is.null(input$pays)) {
      datajo_hote <- datajo2[datajo2$Team == input$pays, ]
      
      datajo_hote <- datajo_hote %>%
        mutate(Medal_Gold = as.numeric(Medal == "Gold"),
               Medal_Silver = as.numeric(Medal == "Silver"),
               Medal_Bronze = as.numeric(Medal == "Bronze"))
      
      medal_count2 <- datajo_hote %>%
        group_by(Team, Year, Season, Host.country, City) %>%
        summarise(
          Medal_Gold = sum(Medal_Gold, na.rm = TRUE),
          Medal_Silver = sum(Medal_Silver, na.rm = TRUE),
          Medal_Bronze = sum(Medal_Bronze, na.rm = TRUE),
          total_Medals = sum(Medal_Gold, Medal_Silver, Medal_Bronze, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        replace_na(list(Medal_Gold = 0, Medal_Silver = 0, Medal_Bronze = 0, total_Medals = 0))
      
      p <- plot_ly(
        x = ~medal_count2$Year,
        type = 'bar',
        hoverinfo = 'text',
        text = paste("Année: ", medal_count2$Year,
                     "<br>Médailles d'or: ", medal_count2$Medal_Gold,
                     "<br>Médailles d'argent: ", medal_count2$Medal_Silver,
                     "<br>Médailles de bronze: ", medal_count2$Medal_Bronze,
                     "<br>Pays hôte: ", input$pays,
                     "<br>Ville: ", medal_count2$City)
      )
      
      p <- p %>% add_trace(
        y = ~medal_count2$Medal_Gold,
        name = 'Or',
        marker = list(color = 'gold', 
                      line = list(color = ifelse(medal_count2$Year %in% datajo_hote$Year[datajo_hote$Host.country == input$pays], 'purple','transparent'), width = 2)
        )) %>%
        add_trace(
          y = ~medal_count2$Medal_Silver,
          name = 'Argent',
          marker = list(color = 'silver',
                        line = list(color = ifelse(medal_count2$Year %in% datajo_hote$Year[datajo_hote$Host.country == input$pays], 'purple','transparent'), width = 2)
          )) %>%
        add_trace(
          y = ~medal_count2$Medal_Bronze,
          name = 'Bronze',
          marker = list(color = 'peru',
                        line = list(color = ifelse(medal_count2$Year %in% datajo_hote$Year[datajo_hote$Host.country == input$pays], 'purple','transparent'), width = 2)
          ))
      
      p <- p %>%
        layout(
          title = paste("Nombre total de médailles pour", input$pays),
          xaxis = list(title = "Année (année hôte en violet)"),
          yaxis = list(title = "Nombre de médailles"),
          barmode = 'stack',  # Pour empiler les barres
          hoverlabel = list(bgcolor = 'rgb(235, 235, 235)', font = list(color = 'rgb(145, 145, 145)'))
        )
    }
  })
  
  # Serveur Graphique 3
  output$medal_trend_plot <- renderPlotly({
    datajo3 <- datajo %>%
      mutate(Medal_Gold = as.numeric(Medal == "Gold"),
             Medal_Silver = as.numeric(Medal == "Silver"),
             Medal_Bronze = as.numeric(Medal == "Bronze"))
    
    medal_count3 <- datajo3 %>%
      group_by(Team, Year) %>%
      summarise(
        total_Medals = sum(Medal_Gold, Medal_Silver, Medal_Bronze, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      replace_na(list(Medal_Gold = 0, Medal_Silver = 0, Medal_Bronze = 0, total_Medals = 0))
    
    medal_count3 <- medal_count3 %>%
      filter(Team %in% (medal_count3 %>% filter(Year >= 2010) %>% group_by(Team) %>% filter(sum(total_Medals) > 0) %>% pull(Team)))
    
    medal_count3 <- medal_count3 %>%
      filter(Team != "Russia")
    
    top_countries <- medal_count3 %>%
      group_by(Team) %>%
      summarise(total_Medals = sum(total_Medals)) %>%
      top_n(10, total_Medals) %>%
      pull(Team)
    
    medal_count_top <- medal_count3 %>%
      filter(Team %in% top_countries)
    
    medal_count_top <- as.data.frame(medal_count_top)
    
    medal_count_top$Year <- as.numeric(as.character(medal_count_top$Year))
    
    p <- plot_ly()
    
    for (team in unique(medal_count_top$Team)) {
      team_data <- medal_count_top[medal_count_top$Team == team, ]
      model <- lm(total_Medals ~ Year, data = team_data)
      preds <- data.frame(Year = seq(min(team_data$Year), 2024))
      preds$total_Medals <- predict(model, newdata = preds)
      preds$total_Medals <- ifelse(preds$total_Medals < 0, 0, preds$total_Medals)
      p <- add_trace(p, data = preds, x = ~Year, y = ~total_Medals, type = 'scatter', mode = 'lines', name = team)
    }
    
    p <- p %>%
      layout(
        xaxis = list(title = "Année"),
        yaxis = list(title = "Nombre de médailles"))
    
    p
  })
}

shinyApp(ui = ui, server = server)


##### Pistes d'amélioration application ####

### Pour l'ensemble de l'application shiny, il serait intéressant de développer davantage le design
# de l'interface d'un point de vue esthétique (arrière-plan et thème de couleur notamment).

#### créer une version démo (moins de données) et une version pleine


##### Il existe différentes pistes d'amélioration que j'aurais aimé apporter au code de cette partie.

### retirer "trace 0" du graphique qui empêche sa lisibilité

### Remplir l'arrière plan du hoverlab en violet pour les années où le pays choisi était hôte mais
# quand j'utilisais : "hoverlabel = list(bgcolor = ifelse(medal_count2$Year %in% 
#datajo_hote$Year[datajo_hote$Host.country == input$pays], 'purple', 'rgb(235, 235, 235)'), 
#font = list(color = 'rgb(145, 145, 145)')" cela redefinissait l'hoverlabel par rapport aux
# couleurs des sous-parties des barres (Bronze, Argent et Or)

### Ajouter les données des années manquantes dans la base de données (2018; 2020 et 2022) à partir
#d'une autre base de données. Mais aussi résoudre les problèmes liés à la base de données qui 
#empêchent l'affichage des données pour certains pays (United States, Yougouslavie, ???)

### Pouvoir traduire le nom des pays et des villes en français lorsqu'ils 
# sont appelés par l'utilisateur. De cette façon il n'y aurait pas de mélange de français et d'anglais
# notamment sur l'histogramme et dans la liste déroulante.

### Proposer une prédiction des résultats des JO 2024 de Paris sur la base d'un modèle de régression.
# De cette façon il aurait pu être amusant d'estimer s'il serait probable que la France performe à Paris.
# Ensuite, il aurait pu être intéressant d'essayer de quantifier la corrélation entre les victoires aux JO
#et le fait d'être le pays organisateur. Cela aurait permis d'améliorer la précision de la prédiction.
