library(shiny)
library(dplyr)
library(DT)
library(shinydashboard)
library(plotly)
library(shinyjs)

# Charger les données depuis le fichier CSV
data <- read.csv("games.csv")

# Nettoyer les données
data_cleaned <- data %>%
  na.omit() %>%
  mutate(Year = as.numeric(as.character(Year)))

# Créer un dataframe agrégé par plateforme pour le nombre total de jeux
total_games_by_platform <- data_cleaned %>%
  group_by(Platform) %>%
  summarise(Total_Games = n())

# Créer un dataframe agrégé par plateforme et année pour l'historique
historical_games_by_platform <- data_cleaned %>%
  group_by(Platform, Year) %>%
  summarise(Total_Games = n())

# UI (Interface Utilisateur)
ui_total_games_by_platform <- dashboardPage(
  dashboardHeader(title = "Nombre total de jeux par plateforme"),
  dashboardSidebar(
    tags$head(
      tags$style(
        HTML("
          .box-primary {
            border-top-color: #3c8dbc;
          }
        ")
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    box(
      title = "Nombre total de jeux par plateforme",
      status = "primary",
      solidHeader = TRUE,
      plotlyOutput("total_games_plot"),
      width = 12
    ),
    box(
      title = "Historique du nombre de jeux par année",
      status = "primary",
      solidHeader = TRUE,
      plotlyOutput("historical_games_plot"),
      width = 12,
      selectInput("selected_platform", "Sélectionnez une plateforme:",
                  choices = unique(historical_games_by_platform$Platform))
    )
  )
)

# Serveur
server_total_games_by_platform <- function(input, output, session) {
  # Créer le graphique de la somme totale des jeux par plateforme
  output$total_games_plot <- renderPlotly({
    plot_ly(total_games_by_platform, y = ~Platform, x = ~Total_Games, type = 'bar', orientation = 'h',
            source = "total_games_plot_click", marker = list(color = rainbow(length(unique(total_games_by_platform$Platform))))) %>%
      layout(title = "Nombre total de jeux par plateforme",
             xaxis = list(title = "Nombre total de jeux"),
             yaxis = list(title = "Plateforme")) %>%
      config(displayModeBar = FALSE)  # Pour masquer la barre de mode d'affichage
  })
  
  observe({
    event_data <- event_data("plotly_click", source = "total_games_plot_click")
    if (!is.null(event_data)) {
      selected_platform <- event_data$y
      updateSelectInput(session, "selected_platform", selected = selected_platform)
    }
  })
  
  observe({
    filtered_data <- historical_games_by_platform %>%
      filter(Platform == input$selected_platform)
    
    output$historical_games_plot <- renderPlotly({
      plot_ly(filtered_data, x = ~Year, y = ~Total_Games, type = 'scatter', mode = 'lines+markers',
              marker = list(color = rainbow(length(unique(filtered_data$Year))))) %>%
        layout(title = paste("Historique du nombre de jeux pour la plateforme", input$selected_platform),
               xaxis = list(title = "Année"),
               yaxis = list(title = "Nombre total de jeux")) %>%
        config(displayModeBar = FALSE)  # Pour masquer la barre de mode d'affichage
    })
  })
  
  observeEvent(input$selected_platform, {
    shinyjs::enable("total_games_plot")
  })
}

# Lancer l'application Shiny
shinyApp(ui_total_games_by_platform, server_total_games_by_platform)
