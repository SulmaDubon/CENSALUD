
analisisEspeciesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, checkboxGroupInput(
        ns("variables_especies"),
        "Seleccione las variables para graficar:",
        choices = list(
          "Hembras de Aedes aegypti" = "H_Aeg",
          "Machos de Aedes aegypti" = "M_Aeg",
          "Hembras de Aedes albopictus" = "H_albo",
          "Machos de Aedes albopictus" = "M_albo"
        ),
        selected = c("H_Aeg", "M_Aeg")
      )),
      column(6, uiOutput(ns("selector_municipios")))
    ),
    fluidRow(
      column(12, leafletOutput(ns("mapa_especies"), height = "500px"))
    ),
    fluidRow(
      column(12, actionButton(ns("guardar_mapa"), "Guardar Mapa"))
    )
  )
}




analisisEspecies <- function(input, output, session, datos_relevantes, carpeta_informe) {
  ns <- session$ns
  
  # Leyendas
  variables_leyendas <- list(
    H_Aeg = "Hembras de Aedes aegypti",
    M_Aeg = "Machos de Aedes aegypti",
    H_albo = "Hembras de Aedes albopictus",
    M_albo = "Machos de Aedes albopictus"
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
      selected = municipios # Todos seleccionados por defecto
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
  
  observeEvent(input$guardar_mapa, {
    req(datos_relevantes(), input$variables_especies, input$municipios_filtro, carpeta_informe())
    
    datos <- datos_relevantes()
    
    mapa <- generar_mapa_especies(
      datos = datos,
      variables_especies = input$variables_especies,
      municipios_seleccionados = input$municipios_filtro,
      variables_leyendas = variables_leyendas,
      crear_icono = crear_icono
    )
    
    # Guardar como HTML
    archivo_html <- guardar_mapa_html(
      mapa,
      file.path(carpeta_informe(), "mapas_especies"),
      "Mapa_Especies"
    )
    
    # Guardar como PNG
    guardar_mapa_png(
      archivo_html,
      file.path(carpeta_informe(), "mapas_especies"),
      "Mapa_Especies"
    )
    
    showNotification("Mapa guardado correctamente en HTML y PNG.", type = "message")
  })
  
}

