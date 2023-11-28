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


server <- function(input, output) {
  
  
  datos_filtrados <- reactive({
    filter(datos, pais == input$pais, anio >= input$ano[1] & anio <= input$ano[2])
  })
  
  
  render_lineas <- function(metrica, titulo) {
    ggplot(datos_filtrados(), aes(x = anio, y = get(metrica), group = pais, color = pais)) +
      geom_line() +
      ggtitle(paste(titulo, "-", input$metrica))
  }
  
  
  render_barras <- function(metrica, titulo) {
    ggplot(datos_filtrados(), aes(x = anio, y = get(metrica), fill = pais)) +
      geom_bar(stat = "identity", position = "dodge") +
      ggtitle(paste(titulo, "-", input$metrica))
  }
  
  
  output$plotHumanaLineas <- renderPlot({
    metrica <- ifelse(input$metrica == "Puntaje", "libertad_humana_puntaje", "libertad_humana_ranking")
    render_lineas(metrica, "Libertad Humana")
  })
  
  output$plotHumanaBarras <- renderPlot({
    metrica <- ifelse(input$metrica == "Puntaje", "libertad_humana_puntaje", "libertad_humana_ranking")
    render_barras(metrica, "Libertad Humana")
  })
  

  output$plotPersonalLineas <- renderPlot({
    metrica <- ifelse(input$metrica == "Puntaje", "libertad_personal_puntaje", "libertad_personal_ranking")
    render_lineas(metrica, "Libertad Personal")
  })
  
  output$plotPersonalBarras <- renderPlot({
    metrica <- ifelse(input$metrica == "Puntaje", "libertad_personal_puntaje", "libertad_personal_ranking")
    render_barras(metrica, "Libertad Personal")
  })
  
  
  output$plotEconomicaLineas <- renderPlot({
    metrica <- ifelse(input$metrica == "Puntaje", "libertad_economica_puntaje", "libertad_economica_ranking")
    render_lineas(metrica, "Libertad Económica")
  })
  
  output$plotEconomicaBarras <- renderPlot({
    metrica <- ifelse(input$metrica == "Puntaje", "libertad_economica_puntaje", "libertad_economica_ranking")
    render_barras(metrica, "Libertad Económica")
  })
  
 
  output$descargarPDF <- downloadHandler(
    filename = function() {paste("datos_", input$pais, ".pdf", sep = "")},
    content = function(file) {
      metrica <- ifelse(input$metrica == "Puntaje", paste("libertad", input$metrica, sep = "_"), paste("libertad", input$metrica, sep = "_"))
      pdf(file)
      
    
      if (input$metrica == "Puntaje") {
        print(render_lineas("libertad_humana_puntaje", "Libertad Humana"))
        print(render_barras("libertad_humana_puntaje", "Libertad Humana"))
      } else {
        print(render_lineas("libertad_humana_ranking", "Libertad Humana"))
        print(render_barras("libertad_humana_ranking", "Libertad Humana"))
      }
      
 
      if (input$metrica == "Puntaje") {
        print(render_lineas("libertad_personal_puntaje", "Libertad Personal"))
        print(render_barras("libertad_personal_puntaje", "Libertad Personal"))
      } else {
        print(render_lineas("libertad_personal_ranking", "Libertad Personal"))
        print(render_barras("libertad_personal_ranking", "Libertad Personal"))
      }
      
    
      if (input$metrica == "Puntaje") {
        print(render_lineas("libertad_economica_puntaje", "Libertad Económica"))
        print(render_barras("libertad_economica_puntaje", "Libertad Económica"))
      } else {
        print(render_lineas("libertad_economica_ranking", "Libertad Económica"))
        print(render_barras("libertad_economica_ranking", "Libertad Económica"))
      }
      
      dev.off()
    }
  )
}


shinyApp(ui, server)
