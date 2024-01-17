# Charger les packages nécessaires
library(shiny)
library(dplyr)
library(DT)  # Pour la table interactive
library(plotly)  # Pour le graphique interactif

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
ui_platform <- fluidPage(
  titlePanel("Nombre total de jeux par plateforme selon l'année"),
  
  # Sélectionner une plateforme
  selectInput("platform", "Sélectionnez une plateforme:",
              choices = unique(data_cleaned$Platform)),
  
  # Afficher la table des données
  DTOutput("table_platform"),
  
  # Afficher le graphique interactif à barres
  plotlyOutput("bar_chart_platform")
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
  
  # Créer le graphique interactif à barres avec plotly
  output$bar_chart_platform <- renderPlotly({
    plot_ly(data_platform %>% filter(Platform == input$platform),
            x = ~Year, y = ~Total_Games, type = 'bar', marker = list(color = ~Platform)) %>%
      layout(title = paste("Nombre total de jeux pour la plateforme", input$platform, "par année"),
             xaxis = list(title = "Année"),
             yaxis = list(title = "Nombre total de jeux"),
             showlegend = TRUE)
  })
}

# Lancer l'application Shiny pour la plateforme
shinyApp(ui_platform, server_platform)
