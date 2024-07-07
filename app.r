library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(shinythemes)

# UI de la aplicación Shiny
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Plataforma de Monitoreo Ambiental"),
  navbarPage(
    title = "Plataforma de Monitoreo Ambiental",
    tabPanel("Inicio",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Cargar archivo CSV:", accept = ".csv"),
                 dateRangeInput("date_range", "Seleccione rango de fechas:", start = NULL, end = NULL),
                 selectInput("variable", "Seleccione variable a visualizar:", choices = NULL),
                 selectInput("ubicacion", "Seleccione ubicación geográfica:", choices = NULL)
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Gráficos", plotOutput("plot")),
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
               p("Esta plataforma permite el monitoreo y visualización interactiva de datos ambientales."),
               p("Los usuarios pueden cargar sus propios archivos CSV, aplicar filtros y visualizar los datos a través de gráficos y mapas interactivos."),
               h3("Autores:"),
               tags$ul(
                 tags$li("Erick Vasquez - contact@erickvasm.com"),
                 tags$li("Luis Amaya - luis@example.com"),
                 tags$li("Jose - jose@example.com")
               )
             )
    )
  ),
  tags$footer(
    style = "text-align:center; padding:10px; background:#4e5d6c; position:fixed; bottom:0; width:100%",
    "Desarrollado por Erick Vasquez, Luis Amaya, Jose"
  )
)

# Server de la aplicación Shiny
server <- function(input, output, session) {

  # Cargar datos desde archivo CSV
  dataset <- reactive({
    req(input$file)
    data <- tryCatch(
      read.csv(input$file$datapath),
      error = function(e) {
        showNotification("Error al cargar el archivo CSV. Asegúrese de que el archivo esté en el formato correcto.", type = "error")
        NULL
      }
    )
    return(data)
  })

  # Actualizar opciones de variable según datos cargados
  observe({
    data <- dataset()
    req(data)
    updateSelectInput(session, "variable", choices = names(data))
    if ("ubicacion" %in% names(data)) {
      updateSelectInput(session, "ubicacion", choices = unique(data$ubicacion))
    }
  })

  # Generación de gráfico dinámico según selección de variables y ubicación
  output$plot <- renderPlot({
    data <- dataset()
    req(data, input$variable)
    ggplot(data, aes_string(x = input$variable)) +
      geom_bar() +
      labs(title = paste("Gráfico de", input$variable),
           x = input$variable, y = "Conteo")
  })

  # Generación de mapa interactivo
  output$map <- renderLeaflet({
    data <- dataset()
    req(data)
    leaflet(data) %>%
      addTiles() %>%
      setView(lng = -70, lat = -33, zoom = 5) %>%
      addMarkers(lng = ~longitud, lat = ~latitud, popup = ~ubicacion)
  })

  # Descarga de datos filtrados
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("datos_filtrados_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- dataset()
      req(data, input$date_range)
      filtered_data <- filter(data, as.Date(fecha) >= input$date_range[1] & as.Date(fecha) <= input$date_range[2])
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )

  # Generación de informe PDF
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("informe_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      pdf(file)
      dev.off()
    }
  )
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
