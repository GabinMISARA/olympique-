source("global.R")
### Serveur

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
    output$graphique_global <- renderPlotly({
      plot_barres <- plot_ly(
        data_filtered(), x = ~Year, color = ~Medal, type = "histogram", 
        colors = c("Gold" = "gold","Silver" = "grey","Bronze" = "darkgoldenrod")) %>%
        layout(title = "Résultats aux JO",
               xaxis = list(title = "Année"),
               yaxis = list(title = "Nombre de médailles"),
               barmode = "stack",
               showlegend = TRUE,
               legend = list(title = "Médaille"))
      return(plot_barres)
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
  
  output$interactivePlot3 <- renderPlotly({
    req(input$pays)
    
    if (!is.null(input$pays)) {
      datajo_Hote <- datajo[datajo$Team == input$pays, ]
      
      datajo_Hote <- datajo_Hote %>%
        mutate(Medal_Gold = as.numeric(Medal == "Gold"),
               Medal_Silver = as.numeric(Medal == "Silver"),
               Medal_Bronze = as.numeric(Medal == "Bronze"))
      
      medal_count2 <- datajo_Hote %>%
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
                      line = list(color = ifelse(medal_count2$Year %in% datajo_Hote$Year[datajo_Hote$Host.country == input$pays], 'purple','transparent'), width = 2)
        )) %>%
        add_trace(
          y = ~medal_count2$Medal_Silver,
          name = 'Argent',
          marker = list(color = 'silver',
                        line = list(color = ifelse(medal_count2$Year %in% datajo_Hote$Year[datajo_Hote$Host.country == input$pays], 'purple','transparent'), width = 2)
          )) %>%
        add_trace(
          y = ~medal_count2$Medal_Bronze,
          name = 'Bronze',
          marker = list(color = 'peru',
                        line = list(color = ifelse(medal_count2$Year %in% datajo_Hote$Year[datajo_Hote$Host.country == input$pays], 'purple','transparent'), width = 2)
          ))
      
      p <- p %>%
        layout(
          title = paste("Nombre total de médailles pour", NPE(input$pays), ":"),
          xaxis = list(title = "Année (année hôte en violet)"),
          yaxis = list(title = "Nombre de médailles"),
          barmode = 'stack',  # Pour empiler les barres
          hoverlabel = list(bgcolor = 'rgb(235, 235, 235)', font = list(color = 'rgb(145, 145, 145)'))
        )
    }
  })
  
  # Serveur Graphique 3
  output$interactivePlot4 <- renderPlotly({
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