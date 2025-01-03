source("modulos/Funciones/funciones_cargadatos.R")

# UI del módulo
cargaDatosUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("archivo"), "Cargar archivo CSV o Excel", accept = c(".csv", ".xls", ".xlsx")),
    actionButton(ns("crear_informe"), "Crear carpeta"),
    actionButton(ns("eliminar_informe"), "Eliminar carpeta"),
    textOutput(ns("mensaje")),
    tableOutput(ns("vista_previa"))
  )
}

# Lógica del servidor para el módulo
cargaDatos <- function(input, output, session, datos_completos, carpeta_informe, categorias) {
  observeEvent(input$crear_informe, {
    # Comprobar si se han cargado los datos
    if (is.null(datos_completos())) {
      showNotification("Primero cargue un archivo antes de crear la carpeta.", type = "error")
    } else {
      # Crear la carpeta si los datos están cargados
      carpeta <- crearCarpetaUnica()
      carpeta_informe(carpeta)
      output$mensaje <- renderText({
        paste("Carpeta creada. Carpeta:", carpeta)
      })
    }
  })
  
  observeEvent(input$eliminar_informe, {
    carpeta <- carpeta_informe()
    if (!is.null(carpeta) && eliminarCarpeta(carpeta)) {
      carpeta_informe(NULL)
      output$mensaje <- renderText({
        paste("Carpeta eliminada. Carpeta:", carpeta)
      })
    } else {
      output$mensaje <- renderText({
        "No hay carpeta para eliminar."
      })
    }
  })
  
  observeEvent(input$archivo, {
    req(input$archivo)
    
    extension <- tools::file_ext(input$archivo$name)
    Encuesta <- NULL
    
    if (extension == "csv") {
      Encuesta <- read.csv(input$archivo$datapath, header = TRUE, stringsAsFactors = FALSE)
    } else if (extension %in% c("xls", "xlsx")) {
      Encuesta <- read_excel(input$archivo$datapath, col_names = TRUE)
    } else {
      showNotification("Formato de archivo no soportado.", type = "error")
      return()
    }
    
    # Verificar si las columnas necesarias están presentes
    columnas_faltantes <- setdiff(unlist(categorias), colnames(Encuesta))
    if (length(columnas_faltantes) > 0) {
      showNotification(
        paste("El archivo no contiene la información necesaria. Faltan columnas:", 
              paste(columnas_faltantes, collapse = ", ")), 
        type = "error"
      )
      return()
    }
    
    # Normalizar datos usando las categorías proporcionadas
    Encuesta <- normalizarDatos(Encuesta, categorias)
    datos_completos(Encuesta)
    
    output$vista_previa <- renderTable({
      head(Encuesta)
    })
  })
}






