# Cargar funciones geoespaciales
source("modulos/Funciones/funciones_geoespaciales.R")
source("modulos/Funciones/funciones_cargadatos.R")

# Cargar submódulos
source("modulos/Funciones/funciones_geoespaciales.R")
source("modulos/analisis_geoespacial.R")
source("modulos/analisis_especies.R")
source("modulos/graficos_geo.R")

# UI para Visualización Geoespacial
visualizacionGeoespacialUI <- function(id) {
  ns <- NS(id)
  navlistPanel(
    "Opciones de Visualización",
    tabPanel("Análisis Geoespacial", analisisGeoespacialUI(ns("analisis_geoespacial_ui"))),
    tabPanel("Especies", analisisEspeciesUI(ns("analisis_especies_ui"))),
    tabPanel("Gráficos de Dispersión", graficosGeoUI(ns("graficos_geo_ui"))),
    widths = c(3, 9)
  )
}



# Servidor para Visualización Geoespacial
visualizacionGeoespacial <- function(input, output, session, datos_completos, carpeta_informe) {
  ns <- session$ns
  
  # Reactivo para datos procesados y relevantes
  datos_relevantes <- reactive({
    req(datos_completos())
    # Crear datos relevantes (usando tu función existente)
    datos <- crear_datos_relevantes(datos_completos())
    print("Datos relevantes creados:")
    print(head(datos))
    return(datos)
  })
  
  
  # Llamar a los submódulos y pasarles los datos procesados
  callModule(
    analisisGeoespacial,
    "analisis_geoespacial_ui",
    datos_relevantes = datos_relevantes,  # Asegúrate de que este nombre coincide
    carpeta_informe = carpeta_informe
  )
  
  callModule(
    analisisEspecies,
    "analisis_especies_ui",
    datos_relevantes = datos_relevantes,
    carpeta_informe = carpeta_informe
  )

  callModule(
    graficosGeo,
    "graficos_geo_ui",
    datos_relevantes = datos_relevantes, 
    carpeta_informe = carpeta_informe
  )
  
}
