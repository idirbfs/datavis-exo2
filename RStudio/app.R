# Charger les packages nécessaires
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)  # Pour la table interactive
library(plotly)  # Pour le graphique interactif

# Charger les données depuis le fichier CSV
data <- read.csv("games.csv")

# UI (Interface Utilisateur)
ui <- fluidPage(
  titlePanel("Somme totale des jeux publiés par éditeur selon l'année"),
  
  # Sélectionner un éditeur
  selectInput("publisher", "Sélectionnez un éditeur:",
              choices = unique(data$Publisher)),
  
  # Afficher la table des données
  DTOutput("table"),
  
  # Afficher le graphique à barres
  plotlyOutput("bar_chart")  # Utilisation de plotly pour rendre le graphique interactif
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
    datatable(filtered_data(), options = list(pageLength = 5), selection = 'single')  # Ajout de la sélection interactive
  })
  
  # Créer le graphique à barres interactif avec plotly
  output$bar_chart <- renderPlotly({
    ggplotly(
      ggplot(filtered_data(), aes(x = as.factor(Year), fill = Publisher)) +
        geom_bar(stat = "count") +
        labs(title = paste("Nombre de jeux publiés par", input$publisher, "par année"),
             x = "Année",
             y = "Nombre total de jeux") +
        theme_minimal() +
        theme(legend.position = "top", legend.title = element_blank())
    )
  })
}

# Lancer l'application
shinyApp(ui, server)
