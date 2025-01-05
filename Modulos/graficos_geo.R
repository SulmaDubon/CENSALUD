#source("modulos/Funciones/funciones_geoespaciales.R")  
#source("modulos/Funciones/funciones_cargadatos.R")

graficosGeoUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, selectInput(
        ns("variable_seleccionada"),
        "Seleccione para graficar:",
        choices = NULL  # Se llenará dinámicamente desde el servidor
      )),
      column(4, uiOutput(ns("selector_municipios"))),  # Selector dinámico de municipios
      column(4, checkboxGroupInput(
        ns("filtro_respuesta"),
        "Respuesta:",
        choices = list("Sí" = 1, "No" = 0),
        selected = c(1, 0)  # Mostrar ambas opciones por defecto
      ))
    ),
    fluidRow(
      column(12, plotlyOutput(ns("grafico_dispersion"), height = "500px"))
    ),
    fluidRow(
      column(12, uiOutput(ns("leyenda_municipios")))  # Usar la leyenda generada por tu función
    ),
    fluidRow(
      column(12, actionButton(ns("guardar_grafico"), "Guardar Gráfico"))
    )
    
  )
}



graficosGeo <- function(input, output, session, datos_relevantes, carpeta_informe) {
  ns <- session$ns
  
  # Definir leyendas localmente
  variables_leyendas <- list(
    FamDiag = "Familiar diagnosticado",
    FamHosp = "Familiar hospitalizado",
    Caso_6m = "Casos en últimos 6 meses",
    ZancViv = "Zancudos observados en la vivienda",
    LarvViv = "Larvas observadas en la vivienda",
    fam_entrev = "Familias entrevistadas",
    H_Aeg = "Hembras de Aedes aegypti",
    M_Aeg = "Machos de Aedes aegypti",
    H_albo = "Hembras de Aedes albopictus",
    M_albo = "Machos de Aedes albopictus"
  )
  
  # UI para el selector de municipios
  output$selector_municipios <- renderUI({
    req(datos_relevantes())
    municipios <- unique(datos_relevantes()$Municipio)
    checkboxGroupInput(
      ns("municipios_filtro"),
      "Seleccione Municipios:",
      choices = municipios,
      selected = municipios  # Seleccionar todos por defecto
    )
  })
  
  # Actualizar opciones del selector de variables
  observe({
    updateSelectInput(
      session,
      "variable_seleccionada",
      choices = setNames(names(variables_leyendas), variables_leyendas),
      selected = names(variables_leyendas)[1]
    )
  })
  
  # Generar el gráfico dinámico
  grafico_actual <- reactive({
    req(datos_relevantes(), input$variable_seleccionada, input$municipios_filtro, input$filtro_respuesta)
    
    datos <- datos_relevantes()
    variable <- input$variable_seleccionada
    leyenda <- variables_leyendas[[variable]]
    
    # Filtrar datos
    datos_filtrados <- datos %>%
      filter(Municipio %in% input$municipios_filtro) %>%
      filter(.data[[variable]] %in% as.numeric(input$filtro_respuesta))
    
    req(nrow(datos_filtrados) > 0)
    
    # Crear gráfico de dispersión interactivo con plotly
    plot_ly(
      data = datos_filtrados,
      x = ~Coor_Long,
      y = ~Coor_Lat,
      color = ~factor(.data[[variable]], levels = c(1, 0), labels = c("Sí", "No")),
      colors = c("red", "blue"),
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 10)
    ) %>%
      layout(
        title = leyenda,
        xaxis = list(title = "Longitud"),
        yaxis = list(title = "Latitud"),
        showlegend = TRUE
      )
  })
  
  # Mostrar el gráfico en la UI
  output$grafico_dispersion <- renderPlotly({
    plotly::config(
      grafico_actual(),
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "lasso2d", "select2d", "hoverClosestCartesian", "hoverCompareCartesian",
        "toggleSpikelines", "toImage"
      ),
      modeBarButtonsToAdd = c("zoom2d", "pan2d", "autoScale2d", "resetScale2d")
    )
  })
  
  # Guardar el gráfico cuando se presiona el botón
  observeEvent(input$guardar_grafico, {
    req(datos_relevantes(), input$variable_seleccionada, carpeta_informe())
    
    # Crear carpeta principal si no existe
    carpeta_principal <- carpeta_informe()
    if (is.null(carpeta_principal) || !dir.exists(carpeta_principal)) {
      carpeta_principal <- crearCarpetaUnica()
      carpeta_informe(carpeta_principal)  # Actualizar reactivo
    }
    
    # Crear subcarpeta para gráficos de dispersión
    subcarpeta <- file.path(carpeta_principal, "graficos_dispersion")
    if (!dir.exists(subcarpeta)) {
      dir.create(subcarpeta, recursive = TRUE)
    }
    
    # Generar un nombre único para el archivo
    nombre_archivo <- generar_nombre_unico(
      variable_leyenda = variables_leyendas[[input$variable_seleccionada]],
      municipios = input$municipios_filtro
    )
    
    # Guardar el gráfico como HTML
    archivo_html <- file.path(subcarpeta, paste0(nombre_archivo, ".html"))
    htmlwidgets::saveWidget(grafico_actual(), archivo_html, selfcontained = TRUE)
    
    # Guardar el gráfico como PNG
    archivo_png <- file.path(subcarpeta, paste0(nombre_archivo, ".png"))
    if (!webshot::is_phantomjs_installed()) webshot::install_phantomjs()
    webshot::webshot(archivo_html, file = archivo_png)
    
    # Notificar éxito
    showNotification(paste("Gráfico guardado correctamente como:", nombre_archivo), type = "message")
  })
}


