source("Modulos/Funciones/funciones_generales.R")
source("Modulos/Funciones/diccionario_respuestas.R")
# Importar módulos
source("Modulos/carga_datos.R")
source("Modulos/visualizacion_geoespacial.R")
source("Modulos/analisis_descriptivo.R")
source("Modulos/inferencia_estadistica.R")
source("Modulos/regresion_modelos.R")
source("Modulos/analisis_multivariante.R")


categorias <- list(
  Conocimiento = c("CncTrans", "CncSint", "CncCont", "FamDiag", "DiagDengue", "DiagZika", 
                   "DiagChik", "FamHosp", "HospDengue", "HospZika", "HospChik", "MetDiag", 
                   "Caso_6m", "AtenMed", "PruebLab", "AutoMed", "MedNat", "BrotVec", "MedVec"),
  Practicas = c("Mosq_uso", "Mosq_Insec", "Mosq_Peri", "MosqTds", "MFN_5", "MFN5_7", 
                "MFA18_40", "MFA_40", "MosqAguj", "MsqFrcStd", "Malla_Uso", "Repel_Uso", 
                "Charla", "Fum_Com", "Fum_frec", "Ult_Vis_A", "Fum_Hogar", "Fum_MotNo", 
                "Fum_Cree", "Frec_Limp_Dep", "Abat_Uso", "Abat_tiemp"),
  Considerar = c("Inf_prev", "Inf_Foll", "Inf_char", "Inf_tv_rad", "Inf_redsoc", "inf_otro", 
                 "Resp_Soc", "Resp_Alc", "Resp_MINSAL"),
  Familia = c("N_5", "N5_17", "A18_40", "A_40", "Embar"),
  Estructura = c("ParedTipo", "RepelloTipo", "TechoTipo", "Gote", "SueloTipo", "EstrucAdic"),
  AguaDisposicion = c("AguaTipo", "FrecRecAg", "AlmBarril", "AlmCub", "AlmPila", "AlmOtro", 
                      "AlmNing", "CubTipo"),
  Recolecta = c("ZancViv", "LarvViv", "RecBrl", "RecCub", "RecPila", "RecMct", "RecLlnts", 
                "RecOtros", "RecNing", "H_Aeg", "M_Aeg", "H_albo", "M_albo"),
  VDEN = c("DENV_1", "DENV_2", "DENV_3", "DENV_4")
  )

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # Barra superior
  tags$div(
    class = "barra-superior",
    tags$img(src = "Barra.png", alt = "Logo"), # Logo reducido
    tags$h1("Monitoreo del virus del Dengue en San Salvador") # Título al lado del logo
  ),
  
  # Contenido y navegación
  navbarPage(
    title = NULL,
    tabPanel("Inicio",
             fluidRow(
               column(
                 width = 12,
                 tags$div(
                   style = "text-align:center; margin-bottom: 20px;",
                   tags$img(
                     src = "imagen_principal.jpg",
                     width = "100%", height = "auto",
                     style = "max-height: 400px; object-fit: cover; border-radius: 10px;"
                   )
                 )
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 tags$div(
                   style = "padding: 20px; text-align: justify;",
                   tags$h3("Diagnóstico y Serotipificación Molecular de Virus del Dengue (VDEN)"),
                   tags$p(
                     "Este proyecto surge en el 2024 como respuesta a la creciente prevalencia del dengue, 
        la infección arboviral más común en la región, convirtiéndose en prioridad para la salud pública. 
        El objetivo principal de este proyecto es estudiar los serotipos circulantes del virus del dengue (VDEN) 
        a través de la vigilancia de vectores, proporcionando datos clave para el diseño de políticas de salud pública 
        más efectivas y basadas en evidencia. Además, busca fortalecer la capacidad local para gestionar y controlar futuros brotes, 
        protegiendo a la población y disminuyendo la carga sanitaria y económica asociada a esta enfermedad."
                   )
                 )
               )
             )
    ),
    tabPanel("Datos", cargaDatosUI("carga_datos_ui")),
    tabPanel("Geoespacial", visualizacionGeoespacialUI("visualizacion_geoespacial_ui")),
    tabPanel("Descriptivo", analisisDescriptivoUI("analisis_descriptivo_ui")),
    tabPanel("Inferencia", inferenciaEstadisticaUI("inferencia_estadistica_ui")),
    tabPanel("Regresión", regresionModelosUI("regresion_modelos_ui")),
    tabPanel("Multivariante", analisisMultivarianteUI("analisis_multivariante_ui"))
  )
)



# Lógica del servidor
server <- function(input, output, session) {
  datos_completos <- reactiveVal()
  carpeta_informe <- reactiveVal()  # Variable reactiva para la carpeta del informe
  
  # Llamadas a los módulos
  callModule(
    cargaDatos,
    "carga_datos_ui",
    datos_completos = datos_completos,
    carpeta_informe = carpeta_informe,
    categorias = categorias
  )
  
  callModule(
    visualizacionGeoespacial,
    "visualizacion_geoespacial_ui",
    datos_completos = datos_completos,
    carpeta_informe = carpeta_informe
  )
  
  callModule(
   analisisDescriptivo,
    "analisis_descriptivo_ui",
    datos_completos = datos_completos,
    carpeta_informe = carpeta_informe,
    categorias = categorias
  )
  
   
   callModule(
     inferenciaEstadistica,
     "inferencia_estadistica_ui",
     datos_completos = datos_completos,
     carpeta_informe = carpeta_informe,
     categorias = categorias
   )
  # 
  # callModule(
  #   regresionModelos,
  #   "regresion_modelos_ui",
  #   datos_completos = datos_completos,
  #   listas_reactivas = listas_reactivas
  # )
  # 
  # callModule(
  #   analisisMultivariante,
  #   "analisis_multivariante_ui",
  #   datos_completos = datos_completos,
  #   listas_reactivas = listas_reactivas
  # )
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)

