library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)

# UI de la aplicación Shiny
ui <- fluidPage(

  # Título de la aplicación
  titlePanel("Plataforma de Monitoreo Ambiental Interactiva"),

  # Sidebar con controles de entrada
  sidebarLayout(
    sidebarPanel(

      # Carga de archivo CSV
      fileInput("file", "Cargar archivo CSV:",
                accept = c(".csv")),

      # Filtros de datos
      dateRangeInput("date_range", "Seleccione rango de fechas:",
                     start = NULL,
                     end = NULL),

      selectInput("variable", "Seleccione variable a visualizar:",
                  choices = NULL),

      # Selección de ubicación geográfica
      selectInput("ubicacion", "Seleccione ubicación geográfica:",
                  choices = NULL)
    ),

    # Panel principal con salida de gráficos y mapas
    mainPanel(
      tabsetPanel(
        tabPanel("Gráficos", plotOutput("plot")),
        tabPanel("Mapas", leafletOutput("map"))
      ),
      downloadButton("download_data", "Descargar Datos"),
      downloadButton("download_report", "Generar Informe PDF")
    )
  )
)

# Server de la aplicación Shiny
server <- function(input, output, session) {

  # Cargar datos desde archivo CSV
  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  # Actualizar opciones de variable según datos cargados
  observe({
    req(dataset())
    updateSelectInput(session, "variable", choices = names(dataset()))
  })

  # Generación de gráfico dinámico según selección de variables y ubicación
  output$plot <- renderPlot({
    req(dataset())
    ggplot(data = dataset(), aes_string(x = input$variable)) +
      geom_bar() +
      labs(title = paste("Gráfico de", input$variable),
           x = input$variable, y = "Conteo")
  })

  # Generación de mapa interactivo
  output$map <- renderLeaflet({
    req(dataset())
    leaflet(data = dataset()) %>%
      addTiles() %>%
      setView(lng = -70, lat = -33, zoom = 5) %>%
      addMarkers(lng = ~longitud, lat = ~latitud,
                 popup = ~ubicacion)
  })

  # Descarga de datos filtrados
  output$download_data <- downloadHandler(
    filename = function() {
      paste("datos_filtrados_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Código para generar y guardar datos filtrados según selección
      filtered_data <- filter(dataset(), fecha >= input$date_range[1] & fecha <= input$date_range[2])
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )

  # Generación de informe PDF
  output$download_report <- downloadHandler(
    filename = function() {
      paste("informe_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Código para generar y guardar informe PDF con gráficos y análisis
      pdf(file)
      # Ejemplo: gráficos y análisis relevantes
      dev.off()
    }
  )
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)

# UI de la aplicación Shiny
ui <- fluidPage(

  # Título de la aplicación
  titlePanel("Plataforma de Monitoreo Ambiental Interactiva"),

  # Sidebar con controles de entrada
  sidebarLayout(
    sidebarPanel(

      # Carga de archivo CSV
      fileInput("file", "Cargar archivo CSV:",
                accept = c(".csv")),

      # Filtros de datos
      dateRangeInput("date_range", "Seleccione rango de fechas:",
                     start = NULL,
                     end = NULL),

      selectInput("variable", "Seleccione variable a visualizar:",
                  choices = NULL),

      # Selección de ubicación geográfica
      selectInput("ubicacion", "Seleccione ubicación geográfica:",
                  choices = NULL)
    ),

    # Panel principal con salida de gráficos y mapas
    mainPanel(
      tabsetPanel(
        tabPanel("Gráficos", plotOutput("plot")),
        tabPanel("Mapas", leafletOutput("map"))
      ),
      downloadButton("download_data", "Descargar Datos"),
      downloadButton("download_report", "Generar Informe PDF")
    )
  )
)

# Server de la aplicación Shiny
server <- function(input, output, session) {

  # Cargar datos desde archivo CSV
  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  # Actualizar opciones de variable según datos cargados
  observe({
    req(dataset())
    updateSelectInput(session, "variable", choices = names(dataset()))
  })

  # Generación de gráfico dinámico según selección de variables y ubicación
  output$plot <- renderPlot({
    req(dataset())
    ggplot(data = dataset(), aes_string(x = input$variable)) +
      geom_bar() +
      labs(title = paste("Gráfico de", input$variable),
           x = input$variable, y = "Conteo")
  })

  # Generación de mapa interactivo
  output$map <- renderLeaflet({
    req(dataset())
    leaflet(data = dataset()) %>%
      addTiles() %>%
      setView(lng = -70, lat = -33, zoom = 5) %>%
      addMarkers(lng = ~longitud, lat = ~latitud,
                 popup = ~ubicacion)
  })

  # Descarga de datos filtrados
  output$download_data <- downloadHandler(
    filename = function() {
      paste("datos_filtrados_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Código para generar y guardar datos filtrados según selección
      filtered_data <- filter(dataset(), fecha >= input$date_range[1] & fecha <= input$date_range[2])
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )

  # Generación de informe PDF
  output$download_report <- downloadHandler(
    filename = function() {
      paste("informe_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Código para generar y guardar informe PDF con gráficos y análisis
      pdf(file)
      # Ejemplo: gráficos y análisis relevantes
      dev.off()
    }
  )
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
