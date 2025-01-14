#------------------------------------------------------
# Función para convertir coordenadas a grados decimales
#------------------------------------------------------
convertir_a_decimal <- function(coordenada) {
  if (is.na(coordenada) || coordenada == "") return(NA_real_)
  
  partes <- as.numeric(unlist(strsplit(coordenada, "[°'\"]")))
  if (length(partes) < 2) return(NA_real_)
  
  decimal <- partes[1] + (partes[2] / 60)
  if (grepl("S|W", coordenada)) decimal <- -decimal
  
  return(decimal)
}

#-------------------------------------------------------
# Función para limpiar coordenadas
#-------------------------------------------------------
limpiar_coordenadas <- function(datos, coord_vars) {
  stopifnot(all(coord_vars %in% names(datos)))
  
  print("Columnas antes de limpiar coordenadas:")
  print(names(datos))
  
  datos <- datos %>%
    mutate(
      Coor_Lat = map_dbl(.data[[coord_vars[1]]], convertir_a_decimal),
      Coor_Long = map_dbl(.data[[coord_vars[2]]], convertir_a_decimal)
    )
  
  print("Columnas después de limpiar coordenadas:")
  print(names(datos))
  
  return(datos)
}


#-------------------------------------------------------
# Función para crear datos relevantes
#-------------------------------------------------------
crear_datos_relevantes <- function(datos) {
  columnas_requeridas <- c("ID", "Municipio", "Coor_Lat", "Coor_Long", 
                           "FamDiag", "FamHosp", "Caso_6m", "ZancViv", 
                           "LarvViv", "H_Aeg", "M_Aeg", "H_albo", 
                           "M_albo", "DENV_1", "DENV_2", "DENV_3", "DENV_4")
  
  # Verifica si faltan columnas
  print("Columnas antes de limpiar:")
  print(names(datos))
  
  datos <- datos %>%
    limpiar_coordenadas(c("Coor_Lat", "Coor_Long")) %>%
    mutate(
      Coor_Long = if_else(Coor_Long > 0, -Coor_Long, Coor_Long), # Ajustar coordenadas
      fam_entrev = 1 # Crear columna fam_entrev con valor constante
    )
  
  # Verificar columnas requeridas presentes
  columnas_presentes <- intersect(columnas_requeridas, colnames(datos))
  
  if (length(columnas_presentes) == 0) {
    stop("No se encontraron columnas requeridas en los datos.")
  }
  
  # Crear una lista final de columnas para selección
  columnas_finales <- c(columnas_presentes, "fam_entrev")
  
  # Seleccionar columnas usando dplyr::select
  datos <- datos %>%
    dplyr::select(all_of(columnas_finales))
  
  print("Columnas después de procesar:")
  print(names(datos))
  
  return(datos)
}


#----------------------------------------------
# Función para crear el mapa interactivo
#----------------------------------------------
crear_mapa <- function(data, variable, leyenda, municipios_seleccionados = NULL) {
  data <- data %>%
    filter(Municipio %in% (municipios_seleccionados %||% unique(data$Municipio)),
           .data[[variable]] == 1)
  
  # Crear ícono personalizado
  icono_personalizado <- makeAwesomeIcon(
    icon = "map-marker",
    markerColor = "red",  # Ajuste de color base
    iconColor = "#ffffff" # Color del ícono interno
  )
  
  leaflet(data) %>%
    addTiles() %>%
    addAwesomeMarkers(
      lng = ~Coor_Long, lat = ~Coor_Lat,
      icon = icono_personalizado,
      popup = ~paste("<strong>ID:</strong>", ID,
                     "<br><strong>Municipio:</strong>", Municipio,
                     "<br><strong>Latitud:</strong>", round(Coor_Lat, 3),
                     "<br><strong>Longitud:</strong>", round(Coor_Long, 3))
    ) %>%
    addLegend(position = "bottomright", colors = "#8b0e13", labels = leyenda, title = "Leyenda") %>%
    addControl(
      html = paste("Municipios seleccionados:", paste(municipios_seleccionados, collapse = ", ")),
      position = "topright"
    )
}



#----------------------------------------------------
# FUNCION OBTENER NOMBRES PARA GUARDAR GRAFICOS
#---------------------------------------------------

# Función para obtener las primeras tres letras sin vocales de cada municipio
procesar_municipio <- function(municipios) {
  municipios <- gsub("[aeiouAEIOU]", "", municipios)  # Eliminar vocales
  municipios <- substr(municipios, 1, 3)  # Tomar las primeras tres letras
  return(municipios)
}

# Función para generar nombre único
generar_nombre_unico <- function(variable_leyenda, municipios) {
  municipios_procesados <- procesar_municipio(municipios)  # Procesar municipios
  municipios_str <- paste(municipios_procesados, collapse = "_")  # Unir municipios con guiones bajos
  paste0(variable_leyenda, "_", municipios_str)  # Combinar leyenda y municipios
}


#------------------------------------------------
# Funcion leyendas personalizadas mapas especies
#-------------------------------------------------
addLegendCustom <- function(map, variables_seleccionadas, colores_denv) {
  # Generar contenido de la leyenda basado en las variables seleccionadas
  legend_html <- paste0(
    "<div style='background-color: white; padding: 5px; border-radius: 5px; font-size: 12px;'>",
    "<strong>Leyenda</strong><br>",
    paste(
      lapply(variables_seleccionadas, function(variable) {
        if (grepl("H_", variable)) {
          # Hembras: círculo con borde amarillo
          color <- ifelse(variable == "H_Aeg", "red", "green")
          paste0(
            "<div style='display: flex; align-items: center; margin-bottom: 5px;'>",
            "<div style='width: 15px; height: 15px; background-color: ", color,
            "; border: 2px solid yellow; border-radius: 50%; margin-right: 5px;'></div>",
            variable,
            "</div>"
          )
        } else if (grepl("M_", variable)) {
          # Machos: triángulos de diferentes colores
          color <- ifelse(variable == "M_Aeg", "red", "green")
          paste0(
            "<div style='display: flex; align-items: center; margin-bottom: 5px;'>",
            "<div style='width: 0; height: 0; border-left: 7px solid transparent; ",
            "border-right: 7px solid transparent; border-bottom: 14px solid ", color, "; ",
            "margin-right: 5px;'></div>",
            variable,
            "</div>"
          )
        } else if (grepl("DENV_", variable)) {
          # Variables DENV: círculos de colores específicos
          color <- colores_denv[[variable]]
          paste0(
            "<div style='display: flex; align-items: center; margin-bottom: 5px;'>",
            "<div style='width: 15px; height: 15px; background-color: ", color,
            "; border: 1px solid black; border-radius: 50%; margin-right: 5px;'></div>",
            variable,
            "</div>"
          )
        } else {
          # Otras variables: ícono predeterminado
          paste0(
            "<div style='display: flex; align-items: center; margin-bottom: 5px;'>",
            "<i class='fa fa-map-marker' style='font-size: 16px; color: red; margin-right: 5px;'></i>",
            variable,
            "</div>"
          )
        }
      }) %>% unlist() %>% paste(collapse = ""),
      "</div>"
    )
  )
  addControl(map, html = legend_html, position = "bottomright")
}


#-------------------------------------------------------
# Funcion para añadir leyenda municipio a mapas especie
#-------------------------------------------------------
addMunicipiosLegend <- function(map, municipios_seleccionados) {
  # Generar contenido de la leyenda para los municipios seleccionados
  legend_html <- paste0(
    "<div style='background-color: white; padding: 5px; border-radius: 5px; font-size: 12px;'>",
    "<strong>Municipios Seleccionados:</strong><br>",
    paste(municipios_seleccionados, collapse = "<br>"),
    "</div>"
  )
  addControl(map, html = legend_html, position = "bottomleft")
}

#-------------------------------------------------
# Funcion para generar mapas para especie
#-------------------------------------------------
generar_mapa_especies <- function(datos, variables_especies, municipios_seleccionados, variables_leyendas, crear_icono) {
  # Definir colores consistentes para DENV
  colores_denv <- list(
    DENV_1 = "blue",   # Azul
    DENV_2 = "purple", # Morado
    DENV_3 = "orange", # Naranja
    DENV_4 = "brown"   # Marrón
  )
  
  mapa <- leaflet() %>% addTiles()
  
  for (variable in variables_especies) {
    # Filtrar datos para la variable actual
    datos_variable <- datos %>%
      filter(!is.na(.data[[variable]]) & .data[[variable]] == 1 & Municipio %in% municipios_seleccionados)
    
    if (nrow(datos_variable) == 0) {
      cat("Advertencia: No hay datos para la variable:", variable, "\n")
      next
    }
    
    if (grepl("H_", variable)) {
      # Graficar Hembras como círculos con borde amarillo
      mapa <- mapa %>%
        addCircleMarkers(
          lng = datos_variable$Coor_Long,
          lat = datos_variable$Coor_Lat,
          color = "yellow",  # Borde amarillo
          fillColor = if_else(variable == "H_Aeg", "red", "green"),
          fillOpacity = 0.6,
          radius = 6,  # Tamaño reducido
          stroke = TRUE,
          weight = 2,
          group = variable,
          popup = paste(
            "<strong>Variable:</strong>", variables_leyendas[[variable]],
            "<br><strong>Municipio:</strong>", datos_variable$Municipio
          )
        )
    } else if (grepl("DENV_", variable)) {
      # Graficar VDen (DENV) como círculos de colores sólidos
      color <- colores_denv[[variable]]
      mapa <- mapa %>%
        addCircleMarkers(
          lng = datos_variable$Coor_Long,
          lat = datos_variable$Coor_Lat,
          color = "black",  # Borde negro
          fillColor = color,
          fillOpacity = 0.8,
          radius = 6,  # Tamaño reducido
          stroke = TRUE,
          weight = 1.5,
          group = variable,
          popup = paste(
            "<strong>Variable:</strong>", variables_leyendas[[variable]],
            "<br><strong>Municipio:</strong>", datos_variable$Municipio
          )
        )
    } else {
      # Graficar otras variables como marcadores con íconos personalizados
      icono <- crear_icono(variable)
      mapa <- mapa %>%
        addMarkers(
          lng = datos_variable$Coor_Long,
          lat = datos_variable$Coor_Lat,
          icon = icono,
          group = variable,
          popup = paste(
            "<strong>Variable:</strong>", variables_leyendas[[variable]],
            "<br><strong>Municipio:</strong>", datos_variable$Municipio
          )
        )
    }
  }
  
  # Agregar leyendas personalizadas y retorno del mapa
  mapa %>%
    addLegendCustom(variables_especies, colores_denv) %>%
    addMunicipiosLegend(municipios_seleccionados)
}

  
  

