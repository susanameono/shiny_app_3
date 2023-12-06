library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(readr)
library(shinyjs)
library(shinyWidgets)  

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "ANÁLISIS DE SPOTIFY", titleWidth = 250),
  
  dashboardSidebar(width = 250,
                   sidebarMenu(
                     selectInput("anyo", "Año:", choices = NULL),
                     selectInput("genero", "Género:", choices = NULL),
                     
                     fluidRow(
                       column(width = 12, offset = 2,
                              downloadButton("descargarDatos", "Descargar Datos", class = "btn-white btn-dark"),
                              useShinyjs()
                       )
                     )
                   )
  ),
  
  dashboardBody(
    column(6, plotlyOutput("grafico")),
    column(6, dataTableOutput("dataTable"))
  )
)

server <- function(input, output, session) {
  
  spotify_data <- reactive({ read.csv2("datos/spotify_2000_2023.csv") })
  
  # Configura las opciones para los selectInput en el servidor
  observe({
    updateSelectInput(session, "anyo", choices = unique(spotify_data()$year))
    updateSelectInput(session, "genero", choices = unique(spotify_data()$top.genre))
  })
  
  genero_anyo <- reactive({
    spotify_data() %>%
      filter(top.genre == input$genero, year == input$anyo)
  })
  
  output$grafico <- renderPlotly({
    ggplot(genero_anyo(), aes(x = duration, y = popularity)) +
      geom_point() +
      labs(title = paste("Popularidad vs Duración"),
           x = "Duración (s)",
           y = "Popularidad") +
      theme(plot.title = element_text(hjust = 0.5))  # Centrar el título
    
  })
  
  output$dataTable <- renderDataTable({
    genero_anyo()
  })
  
  observe({
    if (nrow(genero_anyo()) == 0) {
      shinyjs::disable("descargarDatos")
    } else {
      shinyjs::enable("descargarDatos")
    }
  })
  
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste("spotify_", input$anyo, "_", input$genero, ".csv", sep = "")
    },
    content = function(file) {
      write_csv(genero_anyo(), file)
    } )
}
shinyApp(ui = ui, server = server)