library(shiny); library(leaflet); library(ggplot2); library(dplyr); library(plotly); library(tidyr); library(gtranslate); library(data.table)

###Initialisation

### DATA global
datajo <- fread("https://raw.githubusercontent.com/GabinMISARA/olympique-/main/Dossier%20Code/athlete_events.csv", 
                select = c("ID", "Sex", "Age", "Team", "NOC", "Year", "Season", "City", "Sport", "Medal", "Host.country"), 
                sep = ";")

### Onglet 0 -
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


### Onglet 1 & 2
JO_filt <- datajo[complete.cases(datajo$Medal), ]

first_year <- min(JO_filt$Year, na.rm = TRUE)
# Liste des pays uniques
unique_countries <- unique(JO_filt$NOC)

### Onglet 3 -
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
tableau_final_hote$marker_color <- ifelse(tableau_final_hote$Team == tableau_final_hote$Host.country, "white", "transparent")
tableau_final_hote$marker_size <- ifelse(tableau_final_hote$Team == tableau_final_hote$Host.country, 15, 10)

# Renommer les colonnes
names(tableau_final_hote) <- c("Year", "Season", "Team", "Host.country", "Nb_athletes_Total", "Nb_athletes_Host", "Pourcentage_Participation", "Medal_Gold", "Medal_Silver", "Medal_Bronze", "total_Medals", "marker_color", "marker_size")



### Serveur
# Define server logic required to draw a histogram
function(input, output, session) {
  
  ###Onglet 0 -
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
  ### Onglet 1 -
  data_filtered <- reactive({
    filtered_data <- JO_filt
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
  output$graphique_global <- renderPlotly({
    plot_hist <- plot_ly(
      data_filtered(), x = ~Year, color = ~Medal, type = "histogram", 
      colors = c("Gold" = "gold","Silver" = "grey","Bronze" = "darkgoldenrod")) %>%
      layout(title = "Résultats aux JO",
             xaxis = list(title = "Année"),
             yaxis = list(title = "Nombre de médailles"),
             barmode = "stack",
             showlegend = TRUE,
             legend = list(title = "Médaille"))
    return(plot_hist)
  })
  
  ### Onglet 2 -
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
    return(plot)
  })
  
  ### Onglet 3 -
  
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
}