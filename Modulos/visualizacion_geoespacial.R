# Cargar funciones geoespaciales
source("Modulos/Funciones/funciones_geoespaciales.R")

# Cargar submódulos
source("Modulos/Funciones/funciones_geoespaciales.R")
source("Modulos/analisis_geoespacial.R")
source("Modulos/analisis_especies.R")
source("Modulos/graficos_geo.R")
source("Modulos/vden.R")



# UI para Visualización Geoespacial
visualizacionGeoespacialUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    navlistPanel(
      "Opciones de Visualización",
      tabPanel("Análisis Geoespacial", analisisGeoespacialUI(ns("analisis_geoespacial_ui"))),
      tabPanel("Especies", analisisEspeciesUI(ns("analisis_especies_ui"))),
      tabPanel("Gráficos de Dispersión", graficosGeoUI(ns("graficos_geo_ui"))),
      tabPanel("Visualización DENV", vdenUI(ns("vden_ui"))),
      widths = c(3, 9)
    )
  )
}



# Servidor para Visualización Geoespacial
visualizacionGeoespacial <- function(input, output, session, datos_completos, carpeta_informe) {
  ns <- session$ns
  
  observe({
    req(datos_completos())
    print("Columnas disponibles en datos_completos():")
    print(names(datos_completos()))
  })
  
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
    datos_relevantes = datos_relevantes,  
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
  
  callModule(
    vden,
    "vden_ui",
    datos_relevantes = datos_relevantes,
    carpeta_informe = carpeta_informe
  )
  
  
}
