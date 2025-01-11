# UI del módulo Análisis Geoespacial

analisisGeoespacialUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, selectInput(
        ns("variable_mapa"),
        "Seleccione una variable para graficar:",
        choices = NULL  # Se llenará dinámicamente desde el servidor
      )),
      column(6, checkboxGroupInput(
        ns("municipios_filtro"),
        "Seleccione municipios:",
        choices = NULL  # Se llenará dinámicamente desde el servidor
      ))
    ),
    fluidRow(
      column(12, leafletOutput(ns("mapa_geoespacial"), height = "500px"))
    ),
    fluidRow(
      column(12, actionButton(ns("guardar_grafico"), "Guardar Gráfico"))
    )
  )
}


# Lógica 
analisisGeoespacial <- function(input, output, session, datos_relevantes, carpeta_informe) {
  ns <- session$ns
  
  # Leyendas de las variables
  variables_leyendas <- list(
    FamDiag = "Familiar diagnosticado",
    FamHosp = "Familiar hospitalizado",
    Caso_6m = "Casos en últimos 6 meses",
    ZancViv = "Zancudos observados en la vivienda",
    LarvViv = "Larvas observadas en la vivienda",
    fam_entrev = "Familias entrevistadas",
    DENV_1 = "Presencia de DENV1", 
    DENV_2 = "Presencia de DENV2",
    DENV_3 = "Presencia de DENV3",
    DENV_4 = "PResencia de DENV4"
  )
  
  # Actualizar selectores dinámicos
  observe({
    req(datos_relevantes())
    updateSelectInput(
      session,
      "variable_mapa",
      choices = setNames(names(variables_leyendas), variables_leyendas),
      selected = "fam_entrev"
    )
    municipios <- unique(datos_relevantes()$Municipio)
    updateCheckboxGroupInput(
      session,
      "municipios_filtro",
      choices = municipios,
      selected = municipios
    )
  })
  
  # Renderizar el mapa
  output$mapa_geoespacial <- renderLeaflet({
    req(datos_relevantes(), input$variable_mapa, input$municipios_filtro)
    datos <- datos_relevantes() %>%
      filter(Municipio %in% input$municipios_filtro)
    crear_mapa(
      data = datos,
      variable = input$variable_mapa,
      leyenda = variables_leyendas[[input$variable_mapa]],
      municipios_seleccionados = input$municipios_filtro
    )
  })
  
  # Guardar gráficos
  observeEvent(input$guardar_grafico, {
    carpeta <- carpeta_informe()
    
    if (is.null(carpeta) || !dir.exists(carpeta)) {
      showNotification(
        "La carpeta no existe. Cree una carpeta desde el módulo Carga de Datos antes de guardar.",
        type = "error"
      )
      return()
    }
    
    req(datos_relevantes(), input$variable_mapa, input$municipios_filtro)
    datos <- datos_relevantes() %>%
      filter(Municipio %in% input$municipios_filtro)
    
    # Crear el mapa
    mapa <- crear_mapa(
      data = datos,
      variable = input$variable_mapa,
      leyenda = variables_leyendas[[input$variable_mapa]],
      municipios_seleccionados = input$municipios_filtro
    )
    
    # Crear subcarpeta para mapas
    carpeta_guardado <- file.path(carpeta, "mapas_interactivos")
    if (!dir.exists(carpeta_guardado)) dir.create(carpeta_guardado, recursive = TRUE)
    
    # Generar nombre único del archivo
    nombre_archivo <- generar_nombre_unico(
      variable_leyenda = variables_leyendas[[input$variable_mapa]],
      municipios = input$municipios_filtro
    )
    
    # Guardar como HTML
    archivo_html <- file.path(carpeta_guardado, paste0(nombre_archivo, ".html"))
    htmlwidgets::saveWidget(mapa, archivo_html, selfcontained = TRUE)
    
    # Guardar como PNG
    archivo_png <- file.path(carpeta_guardado, paste0(nombre_archivo, ".png"))
    if (!webshot::is_phantomjs_installed()) webshot::install_phantomjs()
    webshot::webshot(archivo_html, file = archivo_png)
    
    showNotification(
      paste("Gráfico guardado correctamente como:", nombre_archivo),
      type = "message"
    )
  })
}

