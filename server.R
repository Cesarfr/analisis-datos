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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  output$cargaDatos <- renderUI({
    
    if(input$sel){
      
      observeEvent(input$sel, ({
        updateButton(session, "sel", disabled = TRUE)
        shinyjs::disable("tDatos")
      }))
      
      switch (input$tDatos,
        'CSV' = wellPanel(
          fileInput('file1', 'Selecciona un archivo CSV:',
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
                       '"'),
          bsButton(inputId = "subcsv", label = "Enviar", type = "toggle", style = "info")
        ),
        'Manual' = mainPanel(
          h1("Manual")
        )
      )
      
    }
    
  })
})
