
# Función para crear una carpeta única con fecha y número secuencial
crearCarpetaUnica <- function() {
  fecha_actual <- format(Sys.Date(), "%d%m%y")
  numero_carpeta <- 1
  nombre_carpeta <- paste0(fecha_actual, "_", sprintf("%02d", numero_carpeta))
  
  while (dir_exists(nombre_carpeta)) {
    numero_carpeta <- numero_carpeta + 1
    nombre_carpeta <- paste0(fecha_actual, "_", sprintf("%02d", numero_carpeta))
  }
  
  dir_create(nombre_carpeta)
  return(nombre_carpeta)
}

# Función para eliminar una carpeta
eliminarCarpeta <- function(carpeta) {
  if (dir_exists(carpeta)) {
    dir_delete(carpeta)
    return(TRUE)
  }
  return(FALSE)
}

# Función para convertir Unix timestamp a fecha en formato DD/MM/YYYY
convertirUnixTimestamp <- function(timestamp) {
  fecha <- as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")
  return(format(fecha, "%d/%m/%Y"))
}

# Función para normalizar datos cargados
normalizarDatos <- function(Encuesta, categorias) {
  Encuesta <- Encuesta %>%
    mutate(
      across(where(is.character), ~ na_if(.x, "") %>% na_if("NA")),
      across(where(is.factor), ~ as.character(.x) %>% na_if("") %>% na_if("NA") %>% as.factor()),
      across(where(is.numeric), ~ ifelse(.x %in% c("", "NA"), NA, .x))
    )
  
  if ("Fecha" %in% names(Encuesta)) {
    Encuesta$Fecha <- convertirUnixTimestamp(Encuesta$Fecha)
  }
  
  for (grupo in names(categorias)) {
    if (grupo != "Familia") {
      Encuesta[categorias[[grupo]]] <- lapply(Encuesta[categorias[[grupo]]], as.factor)
    } else {
      Encuesta[categorias[[grupo]]] <- lapply(Encuesta[categorias[[grupo]]], as.integer)
    }
  }
  
  return(Encuesta)
}
