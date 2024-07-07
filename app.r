library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(shinythemes)
library(pdftools)  # Librería para generar contenido en el PDF

# UI de la aplicación Shiny
ui <- fluidPage(
  theme = shinytheme("superhero"),
  navbarPage(
    title = "Plataforma de Monitoreo Ambiental",
    tabPanel("Inicio",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Cargar archivo CSV:", accept = c(".csv")),
                 uiOutput("date_ui"), # Selector de rango de fechas dinámico
                 selectInput("variable", "Seleccione variable a visualizar:", choices = NULL),
                 selectInput("plot_type", "Seleccione tipo de gráfico:", 
                             choices = list("Barras" = "bar", "Líneas" = "line", "Puntos" = "point", "Cajas" = "boxplot", "Violin" = "violin", "Histograma" = "hist", "Densidad" = "density")),
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Gráficos", plotOutput("plot")),
                 ),
                 br(), br(), br(), br(), br(),
                 downloadButton("download_report", "Generar PDF")
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
    style = "text-align:center; padding:10px; background:#4e5d6c; position:fixed; bottom:0; width:97%",
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
  
  # Actualizar el rango de fechas basado en los datos disponibles
  output$date_ui <- renderUI({
    data <- dataset()
    req(data)
    if ("fecha" %in% names(data)) {
      dateRangeInput("date_range", "Seleccione rango de fechas:",
                     start = min(as.Date(data$fecha, format="%Y-%m-%d")),
                     end = max(as.Date(data$fecha, format="%Y-%m-%d")),
                     min = min(as.Date(data$fecha, format="%Y-%m-%d")),
                     max = max(as.Date(data$fecha, format="%Y-%m-%d")))
    }
  })
  
  # Generación de gráfico dinámico según selección de variables y tipo de gráfico
  output$plot <- renderPlot({
    data <- dataset()
    req(data, input$variable, input$plot_type)
    
    p <- ggplot(data, aes_string(x = input$variable)) +
      labs(title = paste("Gráfico de", input$variable),
           x = input$variable, y = "Conteo") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar las etiquetas del eje X
    
    if (input$plot_type == "bar") {
      p <- p + geom_bar()
    } else if (input$plot_type == "line") {
      p <- p + geom_line()
    } else if (input$plot_type == "point") {
      p <- p + geom_point()
    } else if (input$plot_type == "boxplot") {
      p <- p + geom_boxplot()
    } else if (input$plot_type == "violin") {
      p <- p + geom_violin()
    } else if (input$plot_type == "hist") {
      p <- p + geom_histogram()
    } else if (input$plot_type == "density") {
      p <- p + geom_density()
    }
    
    print(p)
  }, height = 500, width = 800) # Ajustar el tamaño del gráfico
  
  # Generación de informe PDF
  output$download_report <- downloadHandler(
    filename = function() {
      paste("informe_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Generar contenido para el PDF
      pdf(file)
      cat("Informe de Datos Ambientales\n\n")
      cat("Este informe contiene los datos y gráficos generados desde la plataforma de monitoreo ambiental.\n\n")
      
      # Generar un gráfico de acuerdo a los datos seleccionados
      data <- dataset()
      req(data, input$variable, input$plot_type)
      if (input$plot_type == "bar") {
        p <- ggplot(data, aes_string(x = input$variable)) +
          geom_bar() +
          labs(title = paste("Gráfico de Barras de", input$variable),
               x = input$variable, y = "Conteo")
      } else if (input$plot_type == "line") {
        p <- ggplot(data, aes_string(x = input$variable)) +
          geom_line() +
          labs(title = paste("Gráfico de Líneas de", input$variable),
               x = input$variable, y = "Conteo")
      } else if (input$plot_type == "point") {
        p <- ggplot(data, aes_string(x = input$variable)) +
          geom_point() +
          labs(title = paste("Gráfico de Puntos de", input$variable),
               x = input$variable, y = "Conteo")
      } else if (input$plot_type == "boxplot") {
        p <- ggplot(data, aes_string(x = input$variable)) +
          geom_boxplot() +
          labs(title = paste("Gráfico de Cajas de", input$variable),
               x = input$variable, y = "Conteo")
      } else if (input$plot_type == "violin") {
        p <- ggplot(data, aes_string(x = input$variable)) +
          geom_violin() +
          labs(title = paste("Gráfico de Violín de", input$variable),
               x = input$variable, y = "Conteo")
      } else if (input$plot_type == "hist") {
        p <- ggplot(data, aes_string(x = input$variable)) +
          geom_histogram() +
          labs(title = paste("Histograma de", input$variable),
               x = input$variable, y = "Frecuencia")
      } else if (input$plot_type == "density") {
        p <- ggplot(data, aes_string(x = input$variable)) +
          geom_density() +
          labs(title = paste("Gráfico de Densidad de", input$variable),
               x = input$variable, y = "Densidad")
      }
      print(p)
      dev.off()
    }
  )
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
