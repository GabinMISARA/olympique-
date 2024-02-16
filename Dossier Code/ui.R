source("global.R")
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
  }',
                    "
      .nav-tabs > li > a {
        color: #3b3b3b; /* Couleur du texte des onglets */
        background-color: #CFB095 !important; /* Couleur de fond des onglets (marron clair) */
        border-color: #3b3b3b; /* Couleur de la bordure des onglets */
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: #fff !important; /* Couleur de fond des onglets actifs (blanc) */
        border-color: #3b3b3b; /* Couleur de la bordure des onglets actifs */
      }
    "                 
    ))
    
  ),
  titlePanel("Résultats aux Jeux Olympiques sur 120 ans"),
  tabsetPanel(
    ### Onglet 0 - Accueil, carte
    tabPanel("Nombre de médailles d'or gagné ",
             leafletOutput("map", width = "100%", height = "600px")
    ),
    
    ### Onglet 1 - Histogramme
    tabPanel("Répartition des médailles", titlePanel(" "),
             sidebarLayout(
               sidebarPanel(
                 tags$style(".well {background-color:#CFB095;}"), #change la couleur du fond
                 tags$h4("Paramètres de filtrage"),
                 tags$br(),   # Saut de ligne
                 sliderInput("annee_slider", "Sélectionnez une année :", 
                             min = first_year, 
                             max = max(JO_filt$Year, na.rm = TRUE),
                             value = c(first_year, max(JO_filt$Year, na.rm = TRUE))),
                 selectInput("sport_select", "Sélectionnez un sport :", 
                             choices = c("Tous", unique(JO_filt$Sport))),
                 selectInput("country_select", "Sélectionnez un pays :",
                             choices = c("Tous", unique_countries)),
                 width = 3
               ),
               mainPanel(
                 plotlyOutput("graphique_global"),
                 textOutput("plot_hist_title")
               )
             )
    ),
    
    ### Onglet 2 - Évolution des médailles
    tabPanel("Évolution des médailles", titlePanel(" "),
             
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
    
    ### Onglet 3 - Suivi des performances

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
                     column(width = 12, style = "padding: 20px;", div(plotlyOutput("interactivePlot3"), style = "border: 2px solid #ccc; border-radius: 5px; margin-bottom: 20px;"), div(textOutput("pays_output"))),
                     column(width = 12, style = "padding: 20px;", div(plotlyOutput("interactivePlot4"), style = "border: 2px solid #ccc; border-radius: 5px; margin-bottom: 20px;"))
                   ),
                 )
               )
      )
    )
  )
