vdenUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Visualización de DENV"),
    selectInput(
      ns("denv_seleccion"),
      "Selecciona el serotipo:",
      choices = c("DENV-1", "DENV-2", "DENV-3", "DENV-4")
    ),
    plotOutput(ns("grafico_denv"))
  )
}

vden <- function(input, output, session, datos_relevantes, carpeta_informe) {
  ns <- session$ns
  
  datos_denv <- reactive({
    req(datos_relevantes())
    datos <- datos_relevantes()
    serotipo <- input$denv_seleccion
    datos_filtrados <- datos %>% filter(!!sym(serotipo) == 1)
    return(datos_filtrados)
  })
  
  output$grafico_denv <- renderPlot({
    req(datos_denv())
    datos <- datos_denv()
    
    plot(
      datos$Coor_Lat,
      datos$Coor_Long,
      col = "red",
      pch = 16,
      xlab = "Latitud",
      ylab = "Longitud",
      main = paste("Distribución de", input$denv_seleccion)
    )
  })
}

