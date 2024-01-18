library(shiny)
library(dplyr)
library(DT)
library(shinydashboard)
library(plotly)

# Charger les données depuis le fichier CSV
data <- read.csv("games.csv")

# Nettoyer les données
data_cleaned <- data %>%
  na.omit() %>%
  mutate(Year = as.numeric(as.character(Year)))

# Créer un dataframe agrégé par année et éditeur pour le nombre de jeux
games_by_publisher_year <- data_cleaned %>%
  group_by(Year, Publisher) %>%
  summarise(Total_Games = n())

# UI (Interface Utilisateur)
ui_games_by_publisher_year <- dashboardPage(
  dashboardHeader(title = "Nombre de jeux créés par année par éditeur"),
  dashboardSidebar(),
  dashboardBody(
    box(
      title = "Nombre de jeux créés par année par éditeur",
      status = "primary",
      solidHeader = TRUE,
      width = 12,  # Ajuster la largeur du box
      height = "900px",  # Ajuster la hauteur du box
      plotlyOutput("barflare_plot", height = "800px")  # Ajuster la hauteur du graphique
    )
  )
)

# Serveur
server_games_by_publisher_year <- function(input, output) {
  # Créer le graphique barflare
  output$barflare_plot <- renderPlotly({
    plot_ly(games_by_publisher_year,
            x = ~Year,
            y = ~Total_Games,
            color = ~Publisher,
            type = 'bar') %>%
      layout(title = "Nombre de jeux créés par année par éditeur",
             xaxis = list(title = "Année"),
             yaxis = list(title = "Nombre total de jeux"),
             barmode = 'stack',
             showlegend = TRUE,
             height = 800,  # Ajuster la hauteur du graphique
             width = 1200)  # Ajuster la largeur du graphique
  })
}

# Lancer l'application Shiny
shinyApp(ui_games_by_publisher_year, server_games_by_publisher_year)
