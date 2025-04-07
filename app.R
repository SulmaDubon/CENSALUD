source("Modulos/Funciones/funciones_generales.R")
source("Modulos/Funciones/diccionario_respuestas.R")
# Importar módulos
source("Modulos/carga_datos.R")
source("Modulos/visualizacion_geoespacial.R")
source("Modulos/analisis_descriptivo.R")
source("Modulos/inferencia_estadistica.R")
source("Modulos/tendencia.R")
source("Modulos/modelado.R")

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
  AguaDisposicion = c("ANDA_UtilAgu", "Pozo_UtilAgu", "Rio_UtilAgu", 
                      "Nac_UtilAgu", "Pip_UtilAgu", "Otr_UtilAgu", "FrecRecAg", "AlmBarril", "AlmCub", "AlmPila", "AlmOtro", 
                      "AlmNing", "CubTap", "CubPlast", "CubLam", "CubOtr",
                      "CubNing"),
  Recolecta = c("ZancViv", "LarvViv", "RecBrl", "RecCub", "RecPila", "RecMct", "RecLlnts", 
                "RecOtros", "RecNing", "H_Aeg", "M_Aeg", "H_albo", "M_albo"),
  VDEN = c("Ae_ae_VDEN1", "Ae_ae_VDEN2", "Ae_ae_VDEN3", "Ae_ae_VDEN4", "Ae_alb_VDEN1", "Ae_alb_VDEN2",
           "Ae_alb_VDEN3", "Ae_alb_VDEN4")
)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # Barra superior
  tags$div(
    class = "barra-superior",
    tags$img(src = "Barra.png", alt = "Logo"), # Logo reducido
    tags$h1("Monitoreo del virus del Dengue en El Salvador") # Título al lado del logo
  ),
  
  # Contenido y navegación
  navbarPage(
    title = NULL,
    
    # --- Pestaña INICIO ---
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
             ),
             
             # Botones "Manual" y "Variables" al final de la pestaña Inicio
             fluidRow(
               column(
                 width = 12,
                 tags$div(
                   style = "text-align: center; margin-top: 30px;",
                   actionButton("manual", "Manual de Usuario", style = "margin-right: 20px;"),
                   actionButton("info_variables", "Información de Variables")
                 )
               )
             )
    ),
    
    # --- Otras pestañas ---
    tabPanel("Datos", cargaDatosUI("carga_datos_ui")),
    tabPanel("Geoespacial", visualizacionGeoespacialUI("visualizacion_geoespacial_ui")),
    tabPanel("Descriptivo", analisisDescriptivoUI("analisis_descriptivo_ui")),
    tabPanel("Inferencia", inferenciaEstadisticaUI("inferencia_estadistica_ui")),
    tabPanel("Tendencia", mod_tendencias_ui("tendencias_ui")),
    tabPanel("Modelado", modeladoUI("modelado_ui"))
  ),
  
  # Barra fija con el texto de licencia
  tags$div(
    style = "
      position: fixed; 
      bottom: 0; 
      left: 0; 
      width: 100%; 
      background-color: #f8f9fa; 
      text-align: center; 
      padding: 10px; 
      font-size: 0.8em; 
      color: #666; 
      border-top: 1px solid #ccc;",
    "Esta obra está bajo licencia CC BY-NC-SA 4.0. Para ver una copia de esta licencia, visite https://creativecommons.org/licenses/by-nc-sa/4.0/ © 2 por Sulma Dubon."
  )
)

# Lógica del servidor
server <- function(input, output, session) {
  
  datos_completos <- reactiveVal()
  
  # Llamadas a los módulos
  callModule(
    cargaDatos,
    "carga_datos_ui",
    datos_completos = datos_completos,
    categorias = categorias
  )
  
  #callModule(
   # visualizacionGeoespacial,
    #"visualizacion_geoespacial_ui",
    #datos_completos = datos_completos
    
  #)
  
  #callModule(
   # analisisDescriptivo,
    #"analisis_descriptivo_ui",
    #datos_completos = datos_completos,
    #categorias = categorias
  #)
  
  #callModule(
   # inferenciaEstadistica,
    #"inferencia_estadistica_ui",
    #datos_completos = datos_completos,
    #categorias = categorias
  #)
  
  #callModule(
   # mod_tendencias_server,
    #"tendencias_ui",
    #datos_completos = datos_completos
  #)
  
  #callModule(
   # modelado,
    #"modelado_ui", 
    #datos = datos_completos,
    #categorias = categorias  
  #)
  
  # Acción para abrir el PDF del manual de usuario en un modal
  observeEvent(input$manual, {
    showModal(
      modalDialog(
        title = "Manual de Usuario",
        tags$iframe(
          src = "manual.html",
          width = "100%",
          height = "600px",
          style = "border:none;"
        ),
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
  # Acción para abrir el diccionario de variables (HTML) en un modal
  observeEvent(input$info_variables, {
    showModal(
      modalDialog(
        title = "Información de Variables",
        tags$iframe(
          src = "variables.html",
          width = "100%",
          height = "600px",
          style = "border:none;"
        ),
        easyClose = TRUE,
        size = "l"
      )
    )
  })
}

shinyApp(ui = ui, server = server)


