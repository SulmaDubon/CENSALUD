analisisEspeciesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        6,
        checkboxGroupInput(
          ns("variables_especies"),
          "Seleccione las variables para graficar:",
          choices = list(
            "Hembras de Aedes aegypti" = "H_Aeg",
            "Machos de Aedes aegypti" = "M_Aeg",
            "Hembras de Aedes albopictus" = "H_albo",
            "Machos de Aedes albopictus" = "M_albo",
            "Presencia de DENV1" = "DENV_1",
            "Presencia de DENV2" = "DENV_2",
            "Presencia de DENV3" = "DENV_3",
            "Presencia de DENV4" = "DENV_4"
          ),
          selected = c("H_Aeg", "M_Aeg")
        )
      ),
      column(
        6,
        uiOutput(ns("selector_municipios"))
      )
    ),
    fluidRow(
      column(
        12,
        leafletOutput(ns("mapa_especies"), height = "500px")
      )
    ),
    fluidRow(
      column(6, downloadButton(ns("descargar_html"), "Descargar como HTML")),
      column(6, downloadButton(ns("descargar_png"), "Descargar como PNG"))
    )
  )
}


analisisEspecies <- function(input, output, session, datos_relevantes) {
  ns <- session$ns
  
  # Leyendas
  variables_leyendas <- list(
    H_Aeg = "Hembras de Aedes aegypti",
    M_Aeg = "Machos de Aedes aegypti",
    H_albo = "Hembras de Aedes albopictus",
    M_albo = "Machos de Aedes albopictus",
    DENV_1 = "Presencia de DENV1", 
    DENV_2 = "Presencia de DENV2",
    DENV_3 = "Presencia de DENV3",
    DENV_4 = "Presencia de DENV4"
  )
  
  # Crear un icono para machos (triángulos)
  crear_icono <- function(variable) {
    if (variable == "M_Aeg") {
      makeIcon(
        iconUrl = "www/iconos/triangulo_rojo.png",
        iconWidth = 15,
        iconHeight = 15,
        iconAnchorX = 7.5,
        iconAnchorY = 7.5
      )
    } else if (variable == "M_albo") {
      makeIcon(
        iconUrl = "www/iconos/triangulo_verde.png",
        iconWidth = 15,
        iconHeight = 15,
        iconAnchorX = 7.5,
        iconAnchorY = 7.5
      )
    }
  }
  
  # UI para el selector de municipios
  output$selector_municipios <- renderUI({
    req(datos_relevantes())
    municipios <- unique(datos_relevantes()$Municipio)
    checkboxGroupInput(
      ns("municipios_filtro"),
      "Seleccione Municipios:",
      choices = municipios,
      selected = municipios[1] # Todos seleccionados por defecto
    )
  })
  
  # Renderizar el mapa interactivo
  output$mapa_especies <- renderLeaflet({
    req(datos_relevantes(), input$variables_especies, input$municipios_filtro)
    
    datos <- datos_relevantes()
    
    generar_mapa_especies(
      datos = datos,
      variables_especies = input$variables_especies,
      municipios_seleccionados = input$municipios_filtro,
      variables_leyendas = variables_leyendas,
      crear_icono = crear_icono
    )
  })
  
  # Descargar como HTML
  output$descargar_html <- downloadHandler(
    filename = function() {
      paste0("mapa_especies_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(datos_relevantes(), input$variables_especies, input$municipios_filtro)
      
      datos <- datos_relevantes()
      mapa <- generar_mapa_especies(
        datos = datos,
        variables_especies = input$variables_especies,
        municipios_seleccionados = input$municipios_filtro,
        variables_leyendas = variables_leyendas,
        crear_icono = crear_icono
      )
      
      htmlwidgets::saveWidget(mapa, file, selfcontained = TRUE)
    }
  )
  
  # Descargar como PNG
  output$descargar_png <- downloadHandler(
    filename = function() {
      paste0("mapa_especies_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(datos_relevantes(), input$variables_especies, input$municipios_filtro)
      
      datos <- datos_relevantes()
      mapa <- generar_mapa_especies(
        datos = datos,
        variables_especies = input$variables_especies,
        municipios_seleccionados = input$municipios_filtro,
        variables_leyendas = variables_leyendas,
        crear_icono = crear_icono
      )
      
      # Guardar como HTML temporal
      html_file <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(mapa, html_file, selfcontained = TRUE)
      
      # Convertir a PNG usando webshot
      if (!webshot::is_phantomjs_installed()) webshot::install_phantomjs()
      webshot::webshot(html_file, file = file, delay = 0.2)
    }
  )
}






  

