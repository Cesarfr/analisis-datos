
library(shiny)
library(shinyBS)
library(shinyjs)
library(modeest)
library(DT)
library(RMySQL)

shinyServer(function(input, output, session) {
  inFile <- NULL
  datoscsv <- NULL
  cn <- NULL
  grf <- NULL
  conexionDB <- NULL
  listTables <- NULL
  tableSelected <- NULL
  
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
              'Base de datos' = wellPanel(
                fluidRow(
                  column(
                    8, tags$label("Ingresa los datos de acceso a la base de datos MySQL"),
                    tags$hr(),
                    textInput(inputId = "usuario", label = "Usuario:", placeholder = "Usuario para acceder"),
                    passwordInput(inputId = "passwd", label = "Contraseña:"),
                    textInput(inputId = "hst", label = "Host:", placeholder = "Host", value = "localhost"),
                    textInput(inputId = "nombredb", label = "Nombre de la base de datos:", placeholder = "Nombre de la base de datos"),
                    #bsButton(inputId = "testConn", label = "Probar conexión", style = "success", type = "button"),
                    bsButton(inputId = "subdb", label = "Usar conexion", style = "info", type = "submit")
                  ),
                  column(
                    4, bsAlert("alErrDB"), bsAlert("alSuccDB")
                  )
                )
              ),
              '-- Elige una opcion --' = createAlert(session, "alert", "alErr", title = "Error",
                                                     content = "Escoge una opción para cargar los datos", append = TRUE)
      )
    }
    
  })
  
  # Verificar conexion
  # observeEvent(input$testConn,({
  #   
  #   conexionDB <<- tryCatch(dbConnect(MySQL(), username=input$usuario, password=input$passwd, host=input$hst, dbname=input$nombredb),
  #                           error = function(e){
  #                             createAlert(session, "alErrDB", "alErr", title = "Error",
  #                                         content = "No se ha podido conectar a la base de datos especificada", append = TRUE, style = "danger")
  #                           })
  #   if(typeof(conexionDB) == "S4"){
  #     createAlert(session, "alSuccDB", "alSucc", title = "Success!",
  #                 content = "Conexion éxitosa :)", append = TRUE, style = "success")
  #   }
  # }))
  
  # Conectar a la BD
  observeEvent(input$subdb,({
    if(is.null(conexionDB)){
      tryCatch({
        conexionDB <<- dbConnect(MySQL(), username=input$usuario, password=input$passwd, host=input$hst, dbname=input$nombredb)
        createAlert(session, "conSuccDB", "alSucc", title = "Success!",
                    content = "Conexion éxitosa :)", append = TRUE, style = "success")
        shinyjs::hide("cargar", anim = TRUE)
      },
      error = function(e){
        createAlert(session, "conSuccDB", "alErr", title = "Error",
                    content = "No se ha podido conectar a la base de datos especificada", append = TRUE, style = "danger")
      })
    }else{
      createAlert(session, "conSuccDB", "alSucc", title = "Success!",
                  content = "Conexion éxitosa :)", append = TRUE, style = "success")
      shinyjs::hide("cargar", anim = TRUE)
    }
    dbGetQuery(conexionDB, "SET NAMES utf8")
    checkDB()
  }))
  
  checkDB <- eventReactive(
    input$subdb,
    ({
      output$choiceTableBD <- renderUI({
        listTables <<- dbListTables(conn = conexionDB)
        selectInput(inputId = "selTable", label = "Selecciona una tabla de la base de datos:", choices = listTables)
      })
    })
  )
  
  tableBD <- eventReactive(
    input$selTable,
    ({
      tableSelected <<- input$selTable
      rs <- dbSendQuery(conexionDB, paste0("SELECT * FROM ", tableSelected))
      datos <- dbFetch(rs, n = -1)
      dbClearResult(rs)
      data.frame(datos)
    })
  )
  
  # Debug
  output$debug <- renderText({
    print(conexionDB)
  })
  
  
  # Tabla de datos
  output$tablaDatos <- DT::renderDataTable(DT::datatable({
    
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
      }
    }else if(input$tDatos == "Base de datos" && input$subdb == TRUE){
      datoscsv <<- tableBD()
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
  }), class = "cell-border stripe", extensions = "Responsive")
  
  # UI para el grafico
  output$choicePlot <- renderUI({
    cn <<- names(datoscsv)
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "selPlot", label = "Selecciona los datos a gráficar", choices = cn)
      ),
      mainPanel(
        h3("Gráfico"),
        plotOutput("graf")
      )
    )
  })
  
  # Grafico
  output$graf <- renderPlot({
    grf <<- input$selPlot
    hist(as.double(datoscsv[[grf]]), xlab = grf, ylab = "Frecuencia", main = paste("Histograma de", grf),
         col = '#f59233', border = 'white')
    re()
  })
  
  # Cambio de valores segun lo elegido en el grafico
  re <- eventReactive(input$selPlot, ({
    output$media <- renderText({
      round(mean(as.double(datoscsv[[grf]])), digits = 2)
    })
    output$mediana <- renderText({
      round(median(as.double(datoscsv[[grf]])), digits = 2)
    })
    output$moda <- renderText({
      round(as.numeric(mlv(as.double(datoscsv[[grf]]))[1]), digits = 2)
    })
    output$q1 <- renderText({
      round(quantile(as.double(datoscsv[[grf]]), .25), digits = 2)
    })
    output$q2 <- renderText({
      round(quantile(as.double(datoscsv[[grf]]), .50), digits = 2)
    })
    output$q3 <- renderText({
      round(quantile(as.double(datoscsv[[grf]]), .75), digits = 2)
    })
    output$p10 <- renderText({
      round(quantile(as.double(datoscsv[[grf]]), .10), digits = 2)
    })
    output$p50 <- renderText({
      round(quantile(as.double(datoscsv[[grf]]), .50), digits = 2)
    })
    output$p90 <- renderText({
      round(quantile(as.double(datoscsv[[grf]]), .90), digits = 2)
    })
    
    abline(v = mean(as.double(datoscsv[[grf]])), col = "red")
    abline(v = median(as.double(datoscsv[[grf]])), col = "blue")
    abline(v = mlv(as.double(datoscsv[[grf]]))[1], col = "green")
    legend(x = "topright", c("Media", "Moda", "Mediana"), col = c("red", "blue", "green"), lwd = c(2, 2, 2), bty = "n")
  }))
  
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
                      tags$div(class="panel-body", textOutput("media"))
          )
        ),
        column(
          4, tags$div(class="panel panel-primary",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Mediana"),
                      tags$div(class="panel-body", textOutput("mediana"))
          )
        ),
        column(
          4, tags$div(class="panel panel-primary",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Moda"),
                      tags$div(class="panel-body", textOutput("moda"))
          )
        )
      ),
      fluidRow(
        column(
          12, h3("Cuartiles")
        )
      ),
      fluidRow(
        column(
          4, tags$div(class="panel panel-info",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Cuartil 1"),
                      tags$div(class="panel-body", textOutput("q1"))
          )
        ),
        column(
          4, tags$div(class="panel panel-info",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Cuartil 2"),
                      tags$div(class="panel-body", textOutput("q2"))
          )
        ),
        column(
          4, tags$div(class="panel panel-info",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Cuartil 3"),
                      tags$div(class="panel-body", textOutput("q3"))
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
                      tags$div(class="panel-body", textOutput("p10"))
          )
        ),
        column(
          4, tags$div(class="panel panel-success",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Percentil 50"),
                      tags$div(class="panel-body", textOutput("p50"))
          )
        ),
        column(
          4, tags$div(class="panel panel-success",
                      tags$div(class="panel-heading", tags$h4(class="panel-title"), "Percentil 90"),
                      tags$div(class="panel-body", textOutput("p90"))
          )
        )
      )
    )
    
  })
  
})
