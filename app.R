source("Modulos/Funciones/funciones_generales.R")
source("Modulos/Funciones/diccionario_respuestas.R")
# Importar módulos
source("modulos/carga_datos.R")
source("modulos/visualizacion_geoespacial.R")
source("modulos/analisis_descriptivo.R")
source("modulos/inferencia_estadistica.R")
source("modulos/regresion_modelos.R")
source("modulos/analisis_multivariante.R")


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
                "RecOtros", "RecNing", "H_Aeg", "M_Aeg", "H_albo", "M_albo")
)


# Interfaz del usuario (UI)
ui <- navbarPage(
  "Monitoreo del virus del Dengue en San Salvador",
  
  tabPanel("Carga de Datos", cargaDatosUI("carga_datos_ui")),
  tabPanel("Visualización Geoespacial", visualizacionGeoespacialUI("visualizacion_geoespacial_ui")),
  tabPanel("Análisis Descriptivo", analisisDescriptivoUI("analisis_descriptivo_ui")),
  tabPanel("Inferencia Estadística", inferenciaEstadisticaUI("inferencia_estadistica_ui")),
  tabPanel("Regresión y Modelos", regresionModelosUI("regresion_modelos_ui")),
  tabPanel("Análisis Multivariante", analisisMultivarianteUI("analisis_multivariante_ui"))
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

