library(shiny)
library(dplyr)
library(DT)
library(shinydashboard)
library(plotly)

# Charger les données depuis le fichier CSV
data <- read.csv("games.csv")

# Nettoyer les données et filtrer par année et score
data_cleaned <- data %>%
  na.omit() %>%
  filter(Year == 2015, Critic_Score <= 10, User_Score <= 10) %>%
  select(Name, Critic_Score, User_Score)

# UI (Interface Utilisateur)
ui_score <- dashboardPage(
  dashboardHeader(title = "Scores des jeux"),
  dashboardSidebar(
    tags$head(
      tags$style(
        HTML("
          /* Ajoutez ici vos styles personnalisés */
          .box-primary {
            border-top-color: #3c8dbc;
          }
          .box-title {
            font-size: 18px;
          }
          .box-body {
            overflow-x: auto;
          }
          .box-header .box-title {
            font-size: 18px;
          }
        ")
      )
    )
  ),
  dashboardBody(
    box(
      title = "Scores des jeux",
      status = "primary",
      solidHeader = TRUE,
      plotlyOutput("interactive_plot", height = "600px"),  # Ajustez la hauteur selon vos besoins
      width = 12
    )
  )
)

# Serveur
server_score <- function(input, output) {
  # Créer le graphique à barres groupées interactif pour les scores
  output$interactive_plot <- renderPlotly({
    plot_ly(data_cleaned, x = ~Name, y = ~Critic_Score, type = 'bar', name = 'Critic Score', marker = list(color = 'blue')) %>%
      add_trace(y = ~User_Score, name = 'User Score', marker = list(color = 'green')) %>%
      layout(title = "Scores Critiques et Utilisateurs pour chaque jeu (2015, score <= 10)",
             xaxis = list(title = "Jeux"),
             yaxis = list(title = "Score", range = c(0, 10))) %>%
      config(displayModeBar = FALSE)  # Pour masquer la barre de mode d'affichage
  })
}

# Lancer l'application Shiny pour les scores
shinyApp(ui_score, server_score)
