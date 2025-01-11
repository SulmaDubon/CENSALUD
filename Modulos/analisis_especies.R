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
      column(
        12,
        actionButton(ns("guardar_mapa"), "Guardar Mapa")
      )
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
    M_albo = "Machos de Aedes albopictus",
    DENV_1 = "Presencia de DENV1", 
    DENV_2 = "Presencia de DENV2",
    DENV_3 = "Presencia de DENV3",
    DENV_4 = "PResencia de DENV4"
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
  
  # Acción al presionar el botón "Guardar Mapa"
  observeEvent(input$guardar_mapa, {
    req(datos_relevantes(), input$variables_especies, input$municipios_filtro)
    
    # Validar que la carpeta base existe
    if (is.null(carpeta_informe()) || !dir.exists(carpeta_informe())) {
      showNotification(
        "La carpeta no existe. Cree una carpeta desde el módulo Carga de Datos antes de guardar.",
        type = "error"
      )
      return()
    }
    
    # Crear la carpeta 'mapas_especies' si no existe
    carpeta_mapas <- file.path(carpeta_informe(), "mapas_especies")
    if (!dir.exists(carpeta_mapas)) {
      dir.create(carpeta_mapas, showWarnings = FALSE)
    }
    
    datos <- datos_relevantes()
    
    # Generar el mapa
    mapa <- generar_mapa_especies(
      datos = datos,
      variables_especies = input$variables_especies,
      municipios_seleccionados = input$municipios_filtro,
      variables_leyendas = variables_leyendas,
      crear_icono = crear_icono
    )
    
    # Crear nombres únicos para los archivos pasando los municipios seleccionados
    nombre_base <- generar_nombre_unico("Mapa_Especies", municipios = input$municipios_filtro)
    archivo_html <- file.path(carpeta_mapas, paste0(nombre_base, ".html"))
    archivo_png <- file.path(carpeta_mapas, paste0(nombre_base, ".png"))
    
    # Si el archivo ya existe, agregar un contador
    contador <- 1
    while (file.exists(archivo_html) || file.exists(archivo_png)) {
      contador <- contador + 1
      nombre_base_contador <- paste0(nombre_base, "_", contador)
      archivo_html <- file.path(carpeta_mapas, paste0(nombre_base_contador, ".html"))
      archivo_png <- file.path(carpeta_mapas, paste0(nombre_base_contador, ".png"))
    }
    
    # Guardar como HTML
    htmlwidgets::saveWidget(mapa, archivo_html)
    
    # Guardar como PNG
    webshot::webshot(archivo_html, archivo_png)
    
    showNotification("Mapa guardado correctamente en HTML y PNG.", type = "message")
  })
  
}





  

