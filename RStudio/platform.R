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

# Créer un dataframe agrégé par plateforme et année
data_platform <- data_cleaned %>%
  group_by(Year, Platform) %>%
  summarise(Total_Games = n())

# UI (Interface Utilisateur)
ui_platform <- dashboardPage(
  dashboardHeader(title = "Nombre total de jeux par plateforme selon l'année"),
  dashboardSidebar(
    selectInput("platform", "Sélectionnez une plateforme:", choices = unique(data_cleaned$Platform)),
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
      title = "Tableau des données",
      status = "primary",
      solidHeader = TRUE,
      DTOutput("table_platform"),
      width = 12
    ),
    box(
      title = "Graphique à barres",
      status = "primary",
      solidHeader = TRUE,
      plotlyOutput("bar_chart_platform"),
      width = 12
    )
  )
)

# Serveur
server_platform <- function(input, output) {
  # Filtrer les données en fonction de la plateforme sélectionnée
  filtered_data_platform <- reactive({
    data_cleaned %>%
      filter(Platform == input$platform)
  })
  
  # Afficher la table interactive
  output$table_platform <- renderDT({
    datatable(filtered_data_platform(), options = list(pageLength = 5))
  })
  
  # Créer le graphique à barres avec Plotly
  output$bar_chart_platform <- renderPlotly({
    plot_ly(data_platform %>% filter(Platform == input$platform), 
            x = ~Year, y = ~Total_Games, type = 'bar', name = input$platform) %>%
      layout(title = paste("Nombre total de jeux pour la plateforme", input$platform, "par année"),
             xaxis = list(title = "Année"),
             yaxis = list(title = "Nombre total de jeux"))
  })
}

# Lancer l'application Shiny pour la plateforme
shinyApp(ui_platform, server_platform)
