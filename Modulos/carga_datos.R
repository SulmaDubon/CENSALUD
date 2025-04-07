
# Módulo carga de datos

library(shiny)
library(readxl)
library(dplyr)

# Función: convertir Unix timestamp a fecha legible
convertirUnixTimestamp <- function(timestamp) {
  fecha <- as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")
  return(format(fecha, "%d/%m/%Y"))
}

# Función: normalizar datos
normalizarDatos <- function(Encuesta, categorias) {
  Encuesta <- Encuesta %>%
    mutate(
      across(where(is.character), ~ na_if(.x, "") %>% na_if("NA")),
      across(where(is.factor), ~ as.character(.x) %>% na_if("") %>% na_if("NA") %>% as.factor()),
      across(where(is.numeric), ~ ifelse(.x %in% c("", "NA"), NA, .x))
    )
  
  # Convertir la columna Fecha si existe
  if ("Fecha" %in% names(Encuesta)) {
    Encuesta$Fecha <- convertirUnixTimestamp(Encuesta$Fecha)
  }
  
  # Convertir columnas según grupo de categorías
  for (grupo in names(categorias)) {
    columnas <- intersect(categorias[[grupo]], names(Encuesta))
    if (grupo == "Familia") {
      Encuesta[columnas] <- lapply(Encuesta[columnas], as.integer)
    } else {
      Encuesta[columnas] <- lapply(Encuesta[columnas], as.factor)
    }
  }
  
  return(Encuesta)
}

# UI del módulo
cargaDatosUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("archivo"), "Cargar archivo CSV o Excel", accept = c(".csv", ".xls", ".xlsx")),
    tableOutput(ns("vista_previa"))
  )
}

# Server del módulo
cargaDatos <- function(input, output, session, datos_completos, categorias) {
  observeEvent(input$archivo, {
    req(input$archivo)
    
    extension <- tools::file_ext(input$archivo$name)
    
    # Leer archivo según la extensión
    Encuesta <- switch(
      extension,
      "csv" = read.csv(input$archivo$datapath, header = TRUE, stringsAsFactors = FALSE),
      "xls" = read_excel(input$archivo$datapath, col_names = TRUE),
      "xlsx" = read_excel(input$archivo$datapath, col_names = TRUE),
      {
        showNotification("Formato de archivo no soportado.", type = "error")
        return()
      }
    )
    
    # Verificar columnas faltantes
    columnas_faltantes <- setdiff(unlist(categorias), colnames(Encuesta))
    if (length(columnas_faltantes) > 0) {
      showNotification(
        paste("Advertencia: Faltan columnas:", paste(columnas_faltantes, collapse = ", ")),
        type = "warning"
      )
    }
    
    # Normalizar datos con columnas disponibles
    Encuesta <- normalizarDatos(Encuesta, categorias)
    datos_completos(Encuesta)
    
    # Mostrar vista previa
    output$vista_previa <- renderTable({
      head(Encuesta)
    })
  })
}

