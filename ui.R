
  library(shiny)
  library(shinyBS)
  library(shinyjs)
  #library(modeest)


shinyUI(fluidPage(
    theme = "bootstrap.min.css",
    useShinyjs(),
    # Titulo de la aplicacion
    headerPanel("Análisis estadístico de datos"),
    
    fluidRow(id="cargar",
      column(
        4, wellPanel(
          selectInput("tDatos", "Elige como ingresar datos:", choices = c("-- Elige una opcion --","CSV", "Manual")),
          bsButton(inputId="sel", label = "Seleccionar", style = "success")
        )
      ),
      column(
        8, uiOutput("cargaDatos"), bsAlert("alert")
      )
    ),
    tabsetPanel(
      tabPanel("Tabla", h2("Tabla de los datos"), tableOutput("tablaDatos")),
      tabPanel("Gráfica", plotOutput("distPlot")),
      tabPanel("Cálculos estadísticos", h2("Cálculos estadísticos"), uiOutput("calcEst")),
      tabPanel("Debug", h2("Debug"), textOutput("debug"))
      )
))
