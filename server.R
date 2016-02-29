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
          fileInput('valores', 'Selecciona un archivo CSV:',
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
            tags$textarea(id = "valores", name="valores", cols = 50, rows = 10),
            bsButton(inputId = "subcsv", label = "Enviar", style = "info", type = "submit")
        ),
        '-- Elige una opcion --' = createAlert(session, "alert", "alErr", title = "Error",
                                               content = "Escoge una opción para cargar los datos", append = TRUE)
      )
    }
    
  })
  
  # Tabla de datos
  output$tablaDatos <- renderTable({
    inFile <<- input$valores
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      observeEvent(input$valores, ({
        shinyjs::hide("cargar", anim = TRUE)
      }))
      datoscsv <<- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
               quote=input$quote)
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
          4, tags$div(class="panel panel-primary",
                      tags$div(class="panel-heading", tags$h3(class="panel-title"), "Media"),
                      tags$div(class="panel-body", mean(datoscsv[,1]))
          )
        ),
        column(
          4, tags$div(class="panel panel-primary",
                      tags$div(class="panel-heading", tags$h3(class="panel-title"), "Mediana"),
                      tags$div(class="panel-body", median(datoscsv[,1]))
          )
        ),
        column(
          4, tags$div(class="panel panel-primary",
                      tags$div(class="panel-heading", tags$h3(class="panel-title"), "Moda"),
                      tags$div(class="panel-body", mlv(datoscsv[,1])[1])
          )
        )
      ),
      
      fluidRow(
        column(
          4, tags$div(class="panel panel-info",
                      tags$div(class="panel-heading", tags$h3(class="panel-title"), "Cuartil 1"),
                      tags$div(class="panel-body", quantile(datoscsv[,1], .25))
          )
        ),
        column(
          4, tags$div(class="panel panel-info",
                      tags$div(class="panel-heading", tags$h3(class="panel-title"), "Cuartil 2"),
                      tags$div(class="panel-body", quantile(datoscsv[,1], .50))
          )
        ),
        column(
          4, tags$div(class="panel panel-info",
                      tags$div(class="panel-heading", tags$h3(class="panel-title"), "Cuartil 3"),
                      tags$div(class="panel-body", median(datoscsv[,1], .75))
          )
        )
      ),
      
      fluidRow(
        column(
          4, tags$div(class="panel panel-success",
                      tags$div(class="panel-heading", tags$h3(class="panel-title"), "Percentil 10"),
                      tags$div(class="panel-body", quantile(datoscsv[,1], .10))
          )
        ),
        column(
          4, tags$div(class="panel panel-success",
                      tags$div(class="panel-heading", tags$h3(class="panel-title"), "Percentil 50"),
                      tags$div(class="panel-body", quantile(datoscsv[,1], 0.50))
          )
        ),
        column(
          4, tags$div(class="panel panel-success",
                      tags$div(class="panel-heading", tags$h3(class="panel-title"), "Percentil 90"),
                      tags$div(class="panel-body", median(datoscsv[,1], .90))
          )
        )
      )
    )
    
  })
  
})
