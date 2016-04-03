
  library(shiny)
  library(shinyBS)
  library(shinyjs)
  library(DT)


shinyUI(fluidPage(
    theme = "bootstrap.min.css",
    useShinyjs(),
    # Titulo de la aplicacion
    headerPanel("Análisis estadístico de datos"),
    
    # Area para la carga de datos
    fluidRow(id="cargar",
      column(
        4, wellPanel(
          selectInput("tDatos", "Elige como ingresar datos:", choices = c("-- Elige una opcion --","CSV", "Manual", "Base de datos")),
          bsButton(inputId="sel", label = "Seleccionar", style = "success")
        )
      ),
      column(
        8, uiOutput("cargaDatos"), bsAlert("alert")
      )
    ),
    # Paneles del area de trabajo
    tabsetPanel(
      tabPanel("Tabla", bsAlert("conSuccDB"), h2("Tabla de los datos"), uiOutput("choiceTableBD"), DT::dataTableOutput("tablaDatos")),
      tabPanel("Gráfica", uiOutput("choicePlot")),
      tabPanel("Cálculos estadísticos", h2("Cálculos estadísticos"), uiOutput("calcEst")),
      tabPanel("Debug", h2("Debug"), textOutput("debug"))
      )
))
