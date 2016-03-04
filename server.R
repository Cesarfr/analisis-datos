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
    paste(datoscsv)
  })
  
  
  # Tabla de datos
  output$tablaDatos <- renderTable({
    
    validate(
      need(input$valtxt != "", "")
    )
    
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
      validate(
        need(input$valorescsv != "", "Selecciona datos")
      )
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
  output$distPlot <- renderPlot({
    if (is.null(inFile)){
      return(NULL)
    }else{
      cn <<- names(datoscsv)
      hist(as.double(datoscsv[,1]), xlab = cn[1], ylab = "Frecuencia", main = paste("Histograma de", cn[1]), col = '#f59233', border = 'white')
    }
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
                      tags$div(class="panel-body", mean(as.double(datoscsv[,1])), na.rm = FALSE)
          )
        ),
        column(
          4, tags$div(class="panel panel-primary",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Mediana"),
                      tags$div(class="panel-body", median(as.double(datoscsv[,1])), na.rm = FALSE)
          )
        ),
        column(
          4, tags$div(class="panel panel-primary",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Moda"),
                      tags$div(class="panel-body", mlv(as.double(datoscsv[,1]), na.rm = FALSE)[1])
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
                      tags$div(class="panel-body", quantile(as.double(datoscsv[,1]), .25), na.rm = FALSE)
          )
        ),
        column(
          4, tags$div(class="panel panel-info",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Cuartil 2"),
                      tags$div(class="panel-body", quantile(as.double(datoscsv[,1]), .50), na.rm = FALSE)
          )
        ),
        column(
          4, tags$div(class="panel panel-info",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Cuartil 3"),
                      tags$div(class="panel-body", quantile(as.double(datoscsv[,1]), .75), na.rm = FALSE)
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
                      tags$div(class="panel-body", quantile(as.double(datoscsv[,1]), .10), na.rm = FALSE)
          )
        ),
        column(
          4, tags$div(class="panel panel-success",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Percentil 50"),
                      tags$div(class="panel-body", quantile(as.double(datoscsv[,1]), 0.50), na.rm = FALSE)
          )
        ),
        column(
          4, tags$div(class="panel panel-success",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Percentil 90"),
                      tags$div(class="panel-body", quantile(as.double(datoscsv[,1]), .90), na.rm = FALSE)
          )
        )
      )
    )
    
  })
  
})
