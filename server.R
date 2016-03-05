#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(shinyjs)
library(modeest)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   inFile <- NULL
   datoscsv <- NULL
   cn <- NULL
   grf <- NULL
   
  output$cargaDatos <- renderUI({
    
    if(input$sel){
      observeEvent(input$sel, ({
        if(input$tDatos == "-- Elige una opcion --"){
          updateButton(session, "sel", disabled = FALSE)
          shinyjs::enable("tDatos")
        }else{
          updateButton(session, "sel", disabled = TRUE)
          shinyjs::disable("tDatos")
        }
      }))
      switch (input$tDatos,
        'CSV' = wellPanel(
          fileInput('valorescsv', 'Selecciona un archivo CSV:',
                    accept=c('text/csv', 
                             'text/comma-separated-values,text/plain', 
                             '.csv')),
          tags$hr(),
          checkboxInput('header', 'Cabeceras', TRUE),
          radioButtons('sep', 'Separadores',
                       c(Coma=',',
                         'Punto y coma'=';',
                         Tabulador='\t'),
                       ','),
          radioButtons('quote', 'Comillas',
                       c(Ninguna='',
                         'Comilla doble'='"',
                         'Comilla simple'="'"),
                       '"')
        ),
        'Manual' = wellPanel(
            tags$label("Ingresa los datos separados por comas", name="valores"),
            tags$hr(),
            tags$textarea(id = "valtxt", name="valtxt", cols = 50, rows = 10),
            bsButton(inputId = "subman", label = "Enviar", style = "info", type = "submit")
        ),
        '-- Elige una opcion --' = createAlert(session, "alert", "alErr", title = "Error",
                                               content = "Escoge una opción para cargar los datos", append = TRUE)
      )
    }
    
  })
  
  # Debug
  output$debug <- renderText({
    paste(input$selPlot)
  })
  
  
  # Tabla de datos
  output$tablaDatos <- renderTable({
    
    if(input$tDatos == "Manual" && input$subman == TRUE){
      inFile <<- input$valtxt
      if (is.null(inFile)){
        return(NULL)
      }else{
        observeEvent(input$valtxt, ({
          shinyjs::hide("cargar", anim = TRUE)
        }))
        tmp <- strsplit(inFile, split=",")
        inputValues <- as.numeric(unlist(tmp))
        inputValues[is.na(inputValues)] <- 0
        datoscsv <<- data.frame(inputValues)
        table(datoscsv)
      }
    }else{
      inFile <<- input$valorescsv
      
      if (is.null(inFile)){
        return(NULL)
      }else{
        observeEvent(input$valorescsv, ({
          shinyjs::hide("cargar", anim = TRUE)
        }))
        datoscsv <<- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                              quote=input$quote)
      }
    }
  })
  
  # Grafico
  output$choicePlot <- renderUI({
      cn <<- names(datoscsv)
      wellPanel(
        fluidRow(
          column(
            3, h3("Grafico"),
            selectInput(inputId = "selPlot", label = "Selecciona los datos a graficar", choices = cn)
          ),
          column(
            8, h3("Otrtsad"),
            plotOutput("graf")
          )
        )
      )
  })
  
  output$graf <- renderPlot({
    grf <<- input$selPlot
    hist(as.double(datoscsv[[grf]]), xlab = grf, ylab = "Frecuencia", main = paste("Histograma de", grf),
         col = '#f59233', border = 'white')
  })
  
  
  # Calculos estadisticos
  output$calcEst <- renderUI({
    wellPanel(
      fluidRow(
        column(
          12, h3("Medidas de tendencia central")
        )
      ),
      fluidRow(
        column(
          4, tags$div(class="panel panel-primary",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Media"),
                      tags$div(class="panel-body", round(mean(as.double(datoscsv[[grf]])), digits = 2))
          )
        ),
        column(
          4, tags$div(class="panel panel-primary",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Mediana"),
                      tags$div(class="panel-body", median(as.double(datoscsv[[grf]])))
          )
        ),
        column(
          4, tags$div(class="panel panel-primary",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Moda"),
                      tags$div(class="panel-body", mlv(as.double(datoscsv[[grf]]))[1])
          )
        )
      ),
      fluidRow(
        column(
          12, h3("Cuatriles")
        )
      ),
      fluidRow(
        column(
          4, tags$div(class="panel panel-info",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Cuartil 1"),
                      tags$div(class="panel-body", quantile(as.double(datoscsv[[grf]]), .25))
          )
        ),
        column(
          4, tags$div(class="panel panel-info",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Cuartil 2"),
                      tags$div(class="panel-body", quantile(as.double(datoscsv[[grf]]), .50))
          )
        ),
        column(
          4, tags$div(class="panel panel-info",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Cuartil 3"),
                      tags$div(class="panel-body", quantile(as.double(datoscsv[[grf]]), .75))
          )
        )
      ),
      fluidRow(
        column(
          12, h3("Percentiles")
        )
      ),
      fluidRow(
        column(
          4, tags$div(class="panel panel-success",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Percentil 10"),
                      tags$div(class="panel-body", quantile(as.double(datoscsv[[grf]]), .10))
          )
        ),
        column(
          4, tags$div(class="panel panel-success",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Percentil 50"),
                      tags$div(class="panel-body", quantile(as.double(datoscsv[[grf]]), 0.50))
          )
        ),
        column(
          4, tags$div(class="panel panel-success",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Percentil 90"),
                      tags$div(class="panel-body", round(quantile(as.double(datoscsv[[grf]]), .90), digits = 2))
          )
        )
      )
    )
    
  })
  
})
