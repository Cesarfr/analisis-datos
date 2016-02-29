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
        updateButton(session, "sel", disabled = TRUE)
        shinyjs::disable("tDatos")
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
        'Manual' = mainPanel(
          wellPanel(
            tags$label("Ingresa los datos separados por comas", name="valores"),
            tags$hr(),
            tags$textarea(id = "valores", name="valores", cols = 50, rows = 10),
            bsButton(inputId = "subcsv", label = "Enviar", style = "info", type = "submit")
          )
        )
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
    fluidRow(
      column(
        4, tags$div(class="panel panel-primary",
                    tags$div(class="panel-heading", tags$h3(class="panel-title"), "Media"),
                    tags$div(class="panel-body", mean(datoscsv[,1]))
        )
      ),
      column(
        4, tags$div(class="panel panel-primary",
                    tags$div(class="panel-heading", tags$h3(class="panel-title"), "Moda"),
                    tags$div(class="panel-body", mlv(datoscsv[,1])[1])
        )
      ),
      column(
        4, tags$div(class="panel panel-primary",
                    tags$div(class="panel-heading", tags$h3(class="panel-title"), "Mediana"),
                    tags$div(class="panel-body", median(datoscsv[,1]))
        )
      )
    )
  })
  
})
