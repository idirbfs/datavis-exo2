# Installer les librairies nécessaires
libraries <- c("shiny", "dplyr", "DT", "shinydashboard", "plotly", "shinyjs", "viridis")

for (lib in libraries) {
  if (!requireNamespace(lib, quietly = TRUE)) {
    install.packages(lib)
  }
}

# Charger les librairies
library(shiny)
library(dplyr)
library(DT)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(viridis)

# Charger les données depuis le fichier CSV
data <- read.csv("games.csv")

# Nettoyer les données pour la première partie
data_cleaned <- data %>%
  na.omit() %>%
  mutate(Year = as.numeric(as.character(Year)))

# Créer un dataframe agrégé par plateforme et année
data_platform <- data_cleaned %>%
  group_by(Year, Platform) %>%
  summarise(Total_Games = n())

# Créer un dataframe agrégé par plateforme pour le nombre total de jeux
total_games_by_platform <- data_cleaned %>%
  group_by(Platform) %>%
  summarise(Total_Games = n())

# Créer un dataframe agrégé par plateforme et année pour l'historique
historical_games_by_platform <- data_cleaned %>%
  group_by(Platform, Year) %>%
  summarise(Total_Games = n())

# Créer un dataframe agrégé par année et éditeur pour le nombre de jeux
games_by_publisher_year <- data_cleaned %>%
  group_by(Year, Publisher) %>%
  summarise(Total_Games = n())

# UI (Interface Utilisateur)
ui <- dashboardPage(
  dashboardHeader(title = "Tableau de bord Shiny"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualisation par Plateforme", tabName = "platform_tab"),
      menuItem("Visualisation par Éditeur", tabName = "publisher_tab"),
      menuItem("Visualisation Globale", tabName = "global_tab"),
      menuItem("Scores des jeux", tabName = "score_tab"),
      menuItem("Jeux par Éditeur et Année", tabName = "publisher_year_tab"),
      menuItem("Diagramme 3D", tabName = "scatter3d_tab")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      # Visualisation par Plateforme
      tabItem(
        tabName = "platform_tab",
        h2("Nombre total de jeux par plateforme selon l'année"),
        sidebarPanel(
          selectInput("platform", "Sélectionnez une plateforme:", choices = unique(data_cleaned$Platform))
        ),
        fluidRow(
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
      ),
      
      # Visualisation par Éditeur
      tabItem(
        tabName = "publisher_tab",
        h2("Nombre de jeux publiés par éditeur par année"),
        sidebarPanel(
          selectInput("publisher", "Sélectionnez un éditeur:", choices = unique(data$Publisher))
        ),
        fluidRow(
          box(
            title = "Tableau des données",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("table"),
            width = 12
          ),
          box(
            title = "Graphique à barres",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("bar_chart"),
            width = 12
          )
        )
      ),
      
      # Visualisation Globale
      tabItem(
        tabName = "global_tab",
        h2("Nombre total de jeux par plateforme"),
        sidebarPanel(),
        fluidRow(
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
                        choices = unique(data_cleaned$Platform))
          )
        )
      ),
      
      # Scores des jeux
      tabItem(
        tabName = "score_tab",
        h2("Scores des jeux"),
        box(
          title = "Scores des jeux",
          status = "primary",
          solidHeader = TRUE,
          plotlyOutput("interactive_plot", height = "600px"),
          width = 12
        )
      ),
      
      # Jeux par Éditeur et Année (Nouvel onglet)
      tabItem(
        tabName = "publisher_year_tab",
        h2("Nombre de jeux créés par année par éditeur"),
        box(
          title = "Nombre de jeux créés par année par éditeur",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          height = "900px",
          plotlyOutput("barflare_plot", height = "800px")
        )
      ),
      
      # Diagramme de Dispersion 3D (Nouvel onglet)
      tabItem(
        tabName = "scatter3d_tab",
        h2("Diagramme de Dispersion 3D"),
        box(
          title = "Diagramme de Dispersion 3D",
          status = "primary",
          solidHeader = TRUE,
          plotlyOutput("scatter3d_plot"),
          width = 12
        )
      )
    )
  )
)

    
  


# Serveur
server <- function(input, output, session) {
  # Filtrer les données en fonction de la plateforme sélectionnée pour la première partie
  filtered_data_platform <- reactive({
    data_cleaned %>%
      filter(Platform == input$platform)
  })

  # Afficher la table interactive pour la première partie
  output$table_platform <- renderDT({
    datatable(filtered_data_platform(), options = list(pageLength = 5))
  })

  # Créer le graphique à barres avec Plotly pour la première partie
  output$bar_chart_platform <- renderPlotly({
    plot_ly(data_platform %>% filter(Platform == input$platform),
            x = ~Year, y = ~Total_Games, type = 'bar', name = input$platform) %>%
      layout(title = paste("Nombre total de jeux pour la plateforme", input$platform, "par année"),
             xaxis = list(title = "Année"),
             yaxis = list(title = "Nombre total de jeux"))
  })

  # Filtrer les données en fonction de l'éditeur sélectionné pour la deuxième partie
  filtered_data <- reactive({
    data %>%
      filter(Publisher == input$publisher)
  })

  # Afficher la table interactive pour la deuxième partie
  output$table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 5))
  })

  # Créer le graphique à barres pour la deuxième partie
  output$bar_chart <- renderPlot({
    ggplot(filtered_data(), aes(x = as.factor(Year), fill = Publisher)) +
      geom_bar(stat = "count") +
      labs(title = paste("Nombre de jeux publiés par", input$publisher, "par année"),
           x = "Année",
           y = "Nombre total de jeux") +
      theme_minimal() +
      theme(legend.position = "top", legend.title = element_blank())
  })

  # Créer le graphique à barres groupées interactif pour les scores
  output$interactive_plot <- renderPlotly({
    plot_ly(data_cleaned, x = ~Name, y = ~Critic_Score, type = 'bar', name = 'Critic Score', marker = list(color = 'red')) %>%
      add_trace(y = ~User_Score, name = 'User Score', marker = list(color = 'black')) %>%
      layout(title = "Scores Critiques et Utilisateurs pour chaque jeu ",
             xaxis = list(title = "Jeux"),
             yaxis = list(title = "Score", range = c(0, 10))) %>%
      config(displayModeBar = FALSE)
  })

  # Créer le graphique de la somme totale des jeux par plateforme
  output$total_games_plot <- renderPlotly({
    plot_ly(total_games_by_platform, y = ~Platform, x = ~Total_Games, type = 'bar', orientation = 'h',
            source = "total_games_plot_click", marker = list(color = rainbow(length(unique(total_games_by_platform$Platform))))) %>%
      layout(title = "Nombre total de jeux par plateforme",
             xaxis = list(title = "Nombre total de jeux"),
             yaxis = list(title = "Plateforme")) %>%
      config(displayModeBar = FALSE)
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
        config(displayModeBar = FALSE)
    })
  })

  observeEvent(input$selected_platform, {
    shinyjs::enable("total_games_plot")
  })

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
             height = 800,
             width = 1200)
  })
  # Ajout de la logique pour le Diagramme de Dispersion 3D
  output$scatter3d_plot <- renderPlotly({
    plot_ly(data, x = ~Critic_Score, y = ~User_Score, z = ~Total_Shipped, type = 'scatter3d', mode = 'markers')
  })
}


# Lancer l'application Shiny
shinyApp(ui, server)
