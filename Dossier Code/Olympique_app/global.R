library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinyWidgets)
library(leaflet)
library(gtranslate) 
library(data.table)
library(rsconnect)

## Data Global
datajo <- fread("https://raw.githubusercontent.com/GabinMISARA/olympique-/main/Dossier%20Code/athlete_events.csv", 
                select = c("ID", "Sex", "Age", "Team", "NOC", "Year", "Season", "City", "Sport", "Medal", "Host.country"), 
                sep = ";")

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


#### Onglet 1 ####
# Spécifier l'URL du fichier CSV sur Kaggle
first_year <- min(JO$Year, na.rm = TRUE)
# Liste des pays uniques
unique_countries <- unique(JO$NOC)

#### Onglet 2 ####
JO_filt <- JO[complete.cases(JO$Medal), ]

#### Onglet 1 & 2 ####
JO_filt <- datajo[complete.cases(datajo$Medal), ]


# Fonction pour traduire le nom du pays (NOC) en français avec une gestion des erreurs (aussi dans onglet 3)
NPE <- function(NOC) {
  traduction <- tryCatch(
    {
      translate(paste("for the", unique(JO_filt$Team[JO_filt$NOC == NOC])), to = "fr")
    },
    error = function(e) {NOC})
  return(traduction)
} 

first_year <- min(JO_filt$Year, na.rm = TRUE)
# Liste des pays uniques
unique_countries <- unique(JO_filt$NOC)

#### Onglet 3 - Graphique 1 ####
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



#### Onglet 3 Graphique 2 ####
hote <- unique(datajo$Host.country)
# Compter le nombre de médailles pour chaque équipe

medal_count <- aggregate(Medal ~ Team + Year, data=datajo, function(x) 
  table(factor(x, levels=c("Gold", "Silver", "Bronze"))))


datajo2 <- datajo[, -c(17, 18, 19)] 
hote_alphab <- sort(hote)

# Sélectionner les lignes de datajo2 où la valeur de la colonne "Team" est dans hote_alphab
datajo_Hote <- datajo2[datajo2$Team %in% hote_alphab, ]

# Compter le nombre total de médailles remportées par année
datajo_Hote <- datajo_Hote %>%
  mutate(Medal_Gold = as.numeric(Medal == "Gold"),
         Medal_Silver = as.numeric(Medal == "Silver"),
         Medal_Bronze = as.numeric(Medal == "Bronze"))
medal_count2 <- datajo_Hote %>%
  group_by(Team, Year, Season, Host.country) %>%
  summarise(
    Medal_Gold = sum(Medal_Gold, na.rm = TRUE),
    Medal_Silver = sum(Medal_Silver, na.rm = TRUE),
    Medal_Bronze = sum(Medal_Bronze, na.rm = TRUE),
    total_Medals = sum(Medal_Gold, Medal_Silver, Medal_Bronze, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  replace_na(list(Medal_Gold = 0, Medal_Silver = 0, Medal_Bronze = 0, total_Medals = 0))


#### Onglet 3 - Graphique 3 ####
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