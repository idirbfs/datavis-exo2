library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

# Charger les données depuis le fichier CSV
data <- read.csv("games.csv")

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Tableau de bord Shiny"),
  dashboardSidebar(
    selectInput("publisher", "Sélectionnez un éditeur:", choices = unique(data$Publisher))
  ),
  dashboardBody(
    DTOutput("table"),
    plotOutput("bar_chart")
  )
)
# Serveur
server <- function(input, output) {
  # Filtrer les données en fonction de l'éditeur sélectionné
  filtered_data <- reactive({
    data %>%
      filter(Publisher == input$publisher)
  })
  
  # Afficher la table interactive
  output$table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 5))
  })
  
  # Créer le graphique à barres
  output$bar_chart <- renderPlot({
    ggplot(filtered_data(), aes(x = as.factor(Year), fill = Publisher)) +
      geom_bar(stat = "count") +
      labs(title = paste("Nombre de jeux publiés par", input$publisher, "par année"),
           x = "Année",
           y = "Nombre total de jeux") +
      theme_minimal() +
      theme(legend.position = "top", legend.title = element_blank())
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)
