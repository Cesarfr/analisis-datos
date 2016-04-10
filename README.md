# Aplicación de análisis estadístico de datos

Esta aplicación usa el lenguaje de programación R, el paquete Shiny para crear una aplicación web para el análisis de datos.

## Paquetes necesarios:

* shiny
* shinyjs
* shinyBS
* modeest
* DT
* RMySQL
* agricolae
* plotrix
* knitr
* rmarkdown

## Instalacion de los paquetes necesarios

Para instalar los paquetes necesarios ejecute este comando en la consola de R:

`install.packages(c("shiny", "shinyjs", "shinyBS", "modeest", "DT","RMySQL", "agricolae", "plotrix", "knitr", "rmarkdown"))`

## Métodos de carga de datos

Para trabajar, la aplicación puede manejar datos provenientes de archivos **CSV**, ingreso **manual** de los datos separados por comas y conexión a una base de datos.

## Creación de reportes

La aplicación puede generar reportes en los formatos siguientes:

* PDF
* Word
* HTML

Para la creación de reportes en PDF es necesario tener instalado TeXLive para ello en Ubuntu puede instalarlo ejecutando el siguiente comando

` sudo apt-get install texlive texlive-latex-extra`
