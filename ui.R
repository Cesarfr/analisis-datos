
library(shiny)
library(shinyBS)
library(shinyjs)


shinyUI(fluidPage(
    theme = "bootstrap.min.css",
    useShinyjs(),
    # Titulo de la aplicacion
    headerPanel("Análisis estadístico de datos"),
    
    fluidRow(
      column(
        4, wellPanel(
          selectInput("tDatos", "Elige como ingresar datos:", choices = c("-- Elige una opcion --","CSV", "Manual")),
          bsButton(inputId="sel", label = "Seleccionar", style = "success")
        )
      ),
      column(
        8, uiOutput("cargaDatos")
      )
    )
))
