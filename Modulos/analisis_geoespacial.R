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
      column(6, downloadButton(ns("descargar_html"), "Descargar como HTML")),
      column(6, downloadButton(ns("descargar_png"), "Descargar como PNG"))
    )
  )
}


analisisGeoespacial <- function(input, output, session, datos_relevantes) {
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
    DENV_4 = "Presencia de DENV4"
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
  
  # Descargar como HTML
  output$descargar_html <- downloadHandler(
    filename = function() {
      paste0("mapa_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(datos_relevantes(), input$variable_mapa, input$municipios_filtro)
      datos <- datos_relevantes() %>%
        filter(Municipio %in% input$municipios_filtro)
      
      mapa <- crear_mapa(
        data = datos,
        variable = input$variable_mapa,
        leyenda = variables_leyendas[[input$variable_mapa]],
        municipios_seleccionados = input$municipios_filtro
      )
      
      htmlwidgets::saveWidget(mapa, file, selfcontained = TRUE)
    }
  )
  
  # Descargar como PNG
  output$descargar_png <- downloadHandler(
    filename = function() {
      paste0("mapa_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(datos_relevantes(), input$variable_mapa, input$municipios_filtro)
      datos <- datos_relevantes() %>%
        filter(Municipio %in% input$municipios_filtro)
      
      mapa <- crear_mapa(
        data = datos,
        variable = input$variable_mapa,
        leyenda = variables_leyendas[[input$variable_mapa]],
        municipios_seleccionados = input$municipios_filtro
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




