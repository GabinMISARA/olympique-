library(shiny); library(leaflet); library(ggplot2); library(dplyr); library(plotly); library(tidyr); library(gtranslate)

###Initialisation

### DATA global
JO <- read.csv("https://raw.githubusercontent.com/GabinMISARA/olympique-/ca403a2a076306fa328dfe257c889fd35e2fef4e/Dossier%20Code/athlete_events.csv", sep = ";")

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


### Onglet 1 -
# Spécifier l'URL du fichier CSV sur Kaggle
first_year <- min(JO$Year, na.rm = TRUE)
# Liste des pays uniques
unique_countries <- unique(JO$NOC)


### Onglet 2 -
JO_filt <- JO[complete.cases(JO$Medal), ]


### Onglet 3 -
datajo <- JO
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


### Interface Utilisateur
# Define UI for application that draws a histogram
fluidPage(
  tags$head(
    tags$style(HTML('
  body {
    background-image: url("https://upload.wikimedia.org/wikipedia/commons/thumb/a/a7/Olympic_flag.svg/2560px-Olympic_flag.svg.png");
    background-size: 100px;
    background-position:right top;
    background-repeat: no-repeat;
  }'))
  ),
    titlePanel("Résultats aux Jeux Olympiques sur 120 ans"),
    tabsetPanel(
### Onglet 0 -
      tabPanel("Nombre de médailles d'or gagné ",
               leafletOutput("map", width = "100%", height = "600px")
      ),
      
### Onglet 1 -
      tabPanel("Histogramme", 
               sidebarLayout(
                 sidebarPanel(
                   tags$style(".well {background-color:#CFB095;}"), #change la couleur du fond
                   tags$h4("Paramètres de filtrage"),
                   tags$br(),   # Saut de ligne
                   sliderInput("annee_slider", "Sélectionnez une année :", 
                               min = first_year, 
                               max = max(JO$Year, na.rm = TRUE),
                               value = c(first_year, max(JO$Year, na.rm = TRUE))),
                   tags$br(),   # Saut de ligne
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
  ),

### Onglet 2 -
      tabPanel("Évolution des médailles",
      
      # Mise en page avec un panneau latéral et un panneau principal
      sidebarLayout(
        sidebarPanel(
          tags$style(".well {background-color:#CFB095;}"), #change la couleur du fond
          tags$h4("Paramètres de filtrage"),
          tags$br(),   # Saut de ligne
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
                      selected = "FRA"),
          tags$h6("Cliquer sur la légende sélectionne le type de médaille"),
          width = 3
        ),
        mainPanel(
          plotlyOutput("interactivePlot")
        )
      )
    ),

### Onglet 3 -
      tabPanel("Suivi des performances",   titlePanel("Performances aux Jeux Olympiques"),
               sidebarLayout(
                 sidebarPanel(
                   tags$h4("Paramètres de filtrage"),
                   tags$br(),   # Saut de ligne
                   checkboxGroupInput(inputId = "Saison2", label = "Sélectionnez la saison:", choices = c("Été" = "Summer", "Hiver" = "Winter"), selected = c("Summer", "Winter"), inline = TRUE),
                   tags$br(),
                   selectInput(inputId = "pays2", label = "Sélectionnez un pays:", choices = unique(tableau_final_hote$Team), selected = "Australia"),
                   width = 3
                 ),
                 mainPanel(
                   plotlyOutput("interactivePlot2")
                 )
               )),
      tabPanel("A la maison on est champion ? ", "Contenu de l'onglet 4")
    )
  )