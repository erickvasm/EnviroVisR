library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(shinythemes)

# UI de la aplicacion Shiny
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel(""),
  navbarPage(
    title = "Plataforma de Monitoreo Ambiental",
    tabPanel("Inicio",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Cargar archivo CSV:", accept = c(".csv")),
                 dateRangeInput("date_range", "Seleccione rango de fechas:", start = NULL, end = NULL),
                 selectInput("variable", "Seleccione variable a visualizar:", choices = NULL),
                 selectInput("ubicacion", "Seleccione ubicacion geografica:", choices = NULL)
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Graficos", plotOutput("plot")),
                   tabPanel("Mapas", leafletOutput("map"))
                 ),
                 downloadButton("download_data", "Descargar Datos"),
                 downloadButton("download_report", "Generar Informe PDF")
               )
             )
    ),
    tabPanel("Acerca de",
             fluidPage(
               h2("Acerca de esta plataforma"),
               p("Esta plataforma permite el monitoreo y visualizacion interactiva de datos ambientales. Los usuarios pueden cargar sus propios archivos CSV, aplicar filtros y visualizar los datos a traves de graficos y mapas interactivos.")
             )
    )
  ),
  tags$footer(
    style = "text-align:center; padding:10px; background:#4e5d6c; position:fixed; bottom:0; width:100%",
    "Desarrollado por Erick Vasquez, Luis Amaya, Jose"
  )
)

# Server de la aplicacion Shiny
server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  observe({
    req(dataset())
    updateSelectInput(session, "variable", choices = names(dataset()))
  })
  
  output$plot <- renderPlot({
    req(dataset())
    ggplot(data = dataset(), aes_string(x = input$variable)) +
      geom_bar() +
      labs(title = paste("Grafico de", input$variable),
           x = input$variable, y = "Conteo")
  })
  
  output$map <- renderLeaflet({
    req(dataset())
    leaflet(data = dataset()) %>%
      addTiles() %>%
      setView(lng = -70, lat = -33, zoom = 5) %>%
      addMarkers(lng = ~longitud, lat = ~latitud, popup = ~ubicacion)
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("datos_filtrados_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      filtered_data <- filter(dataset(), fecha >= input$date_range[1] & fecha <= input$date_range[2])
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste("informe_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file)
      dev.off()
    }
  )
}

# Ejecutar la aplicacion Shiny
shinyApp(ui = ui, server = server)
