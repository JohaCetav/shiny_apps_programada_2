# Tarea Programada #2 Shiny
# Johanna Salazar Ramírez

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)


datos <- read.csv("Datos_libertad/datos_libertad.csv")

View(datos)

ui <- dashboardPage(
  dashboardHeader(title = "Libertades Mundiales", titleWidth = 300),
  
  dashboardSidebar(
    selectInput("pais", "Selecciona un País", choices = unique(datos$pais)),
    sliderInput("ano", "Selecciona el año:", min = 2008, max = 2016, value = c(2008, 2016)),
    radioButtons("metrica", "Elige visualización:", choices = c("Puntaje", "Ranking")),
    downloadButton("descargarPDF", "Descargar en PDF")
  ),
  
  dashboardBody(
    tabsetPanel(
      tabPanel("Libertad Humana", 
               plotOutput("plotHumanaLineas"),
               plotOutput("plotHumanaBarras")
      ),
      tabPanel("Libertad Personal", 
               plotOutput("plotPersonalLineas"),
               plotOutput("plotPersonalBarras")
      ),
      tabPanel("Libertad Económica", 
               plotOutput("plotEconomicaLineas"),
               plotOutput("plotEconomicaBarras")
      )
    )
  )
)
