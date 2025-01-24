modeladoUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Primera parte: Selección de variables
    fluidRow(
      column(3, 
             selectInput(ns("variable_dependiente"), "Seleccione Variable Dependiente:", 
                         choices = NULL),
             actionButton(ns("ejecutar_random_forest"), "Ejecutar Random Forest"),
             numericInput(ns("num_variables"), "Número de Variables Automáticas:", value = 5, min = 1)
      ),
      column(9, 
             selectInput(ns("categoria"), "Seleccione Categoría:", choices = NULL),
             uiOutput(ns("variables_ui"))
      )
    ),
    
    # Segunda parte: Lista de variables seleccionadas
    fluidRow(
      column(12, tableOutput(ns("tabla_seleccion")))
    ),
    
    # Tercera parte: Configuración y aplicación del modelo
    fluidRow(
      column(6, checkboxInput(ns("usar_stepwise"), "Usar Stepwise Selection", value = TRUE)),
      column(6, actionButton(ns("aplicar_modelo"), "Aplicar Modelo"))
    ),
    
    # Resultados del modelo
    fluidRow(
      column(12, verbatimTextOutput(ns("resumen_modelo")))
    ),
    
    # Tabla de resultados
    fluidRow(
      column(12, tableOutput(ns("tabla_resultados")))
    ),
    
    # Métricas y visualización
    fluidRow(
      column(6, plotOutput(ns("curva_roc"))),
      column(6, plotOutput(ns("importancia_variables")))
    )
  )
}

modelado <- function(input, output, session, datos_completos, categorias) {
  ns <- session$ns
  
  # Filtrar y limpiar los datos al entrar al módulo
  datos_filtrados_modelado <- reactive({
    req(datos_completos())
    
    variables_excluidas <- c(
      categorias$Considerar, # Eliminar todas las variables de la categoría "Considerar"
      "ID", "Fecha", "DiagDengue", "DiagZika", "DiagChik", "HospDengue", "HospZika", 
      "HospChik", "MetDiag", "AtenMed", "PruebLab", "AutoMed", "MedNat", "MedVec", 
      "Fum_MotNo", "Fum_Cree", "Frec_Limp_Dep", "Abat_Uso", "Abat_tiemp", 
      "RecBrl", "RecCub", "RecPila", "RecMct", "RecLlnts", "RecOtros", 
      "RecNing", "H_Aeg", "M_Aeg", "H_albo", "M_albo"
    )
    
    datos <- datos_completos() %>%
      dplyr::select((-dplyr::all_of(variables_excluidas)), Municipio) %>%
      stats::na.omit()
    
    if (nrow(datos) < 10) {
      shiny::showNotification("Datos insuficientes después de filtrar y eliminar valores faltantes.", type = "error")
      return(NULL)
    }
    
    datos
  })
  
  # Lista de variables dependientes disponibles
  variables_dependientes <- c("FamDiag", "FamHosp", "Caso_6m", "ZancViv", "LarvViv")
  
  # Actualizar selector de variable dependiente
  observe({
    updateSelectInput(session, "variable_dependiente", choices = variables_dependientes)
  })
  
  # Limpiar datos al cambiar la variable dependiente
  observeEvent(input$variable_dependiente, {
    variables_seleccionadas(data.frame(Variable = character()))
    
    output$resumen_modelo <- renderText("")
    output$tabla_resultados <- renderTable(data.frame())
    output$metricas_modelo <- renderText("")
    output$curva_roc <- renderPlot(NULL)
    output$importancia_variables <- renderPlot(NULL)
    
    shiny::showNotification("Variable dependiente cambiada. Datos limpiados.", type = "message")
  })
  
  # Lista reactiva de variables seleccionadas
  variables_seleccionadas <- reactiveVal(data.frame(Variable = character()))
  
  # Actualizar selector de categoría
  observe({
    req(categorias)
    updateSelectInput(session, "categoria", choices = names(categorias))
  })
  
  # Mostrar checkbox de variables dentro de la categoría seleccionada
  output$variables_ui <- renderUI({
    req(input$categoria, categorias, datos_filtrados_modelado())
    variables <- intersect(names(datos_filtrados_modelado()), categorias[[input$categoria]])
    tags$div(
      style = "column-count: 3; column-gap: 20px;",
      checkboxGroupInput(ns("variables"), "Seleccione Variables:", choices = variables, selected = variables_seleccionadas()$Variable)
    )
  })
  
  # Actualizar listado de variables seleccionadas dinámicamente
  observeEvent(input$variables, {
    variables_seleccionadas(data.frame(Variable = input$variables))
  })
  
  # Mostrar listado de variables seleccionadas
  output$tabla_seleccion <- renderTable({
    variables_seleccionadas()
  })
  
  # Ejecutar Random Forest
  observeEvent(input$ejecutar_random_forest, {
    req(datos_filtrados_modelado(), input$variable_dependiente)
    
    print("Ejecutando Random Forest con los datos filtrados...")
    
    datos_rf <- datos_filtrados_modelado()
    
    if (nrow(datos_rf) < 10) {
      shiny::showNotification("Datos insuficientes para ejecutar Random Forest.", type = "error")
      return()
    }
    
    library(randomForest)
    formula_rf <- as.formula(paste(input$variable_dependiente, "~ ."))
    modelo_rf <- randomForest::randomForest(formula_rf, data = datos_rf, importance = TRUE)
    
    importancia_rf <- randomForest::importance(modelo_rf)
    top_variables <- names(sort(importancia_rf[, 1], decreasing = TRUE))[1:input$num_variables]
    
    seleccion_total <- unique(c(variables_seleccionadas()$Variable, top_variables))
    variables_seleccionadas(data.frame(Variable = seleccion_total))
    
    output$importancia_variables <- renderPlot({
      randomForest::varImpPlot(modelo_rf, main = "Importancia de Variables")
    })
    
    shiny::showNotification("Random Forest ejecutado con éxito. Variables importantes añadidas a la lista.", type = "message")
  })
  
  # Aplicar modelo de regresión logística
  observeEvent(input$aplicar_modelo, {
    # Validar que las entradas no sean NULL ni vacías
    if (is.null(input$variable_dependiente) || input$variable_dependiente == "") {
      shiny::showNotification("Por favor, seleccione una variable dependiente.", type = "error")
      return()
    }
    
    if (is.null(variables_seleccionadas()) || nrow(variables_seleccionadas()) == 0) {
      shiny::showNotification("Por favor, seleccione al menos una variable explicativa.", type = "error")
      return()
    }
    
    if (is.null(datos_filtrados_modelado()) || nrow(datos_filtrados_modelado()) == 0) {
      shiny::showNotification("No hay datos disponibles para ajustar el modelo.", type = "error")
      return()
    }
    
    # Seleccionar datos para el modelo
    datos_glm <- datos_filtrados_modelado() %>%
      dplyr::select(dplyr::all_of(c(input$variable_dependiente, variables_seleccionadas()$Variable)))
    
    # Validar cantidad de datos
    if (nrow(datos_glm) < 10) {
      shiny::showNotification("Datos insuficientes para aplicar el modelo de regresión logística.", type = "error")
      return()
    }
    
    # Construir fórmula
    formula_glm <- as.formula(paste(
      input$variable_dependiente, 
      "~", 
      paste(variables_seleccionadas()$Variable, collapse = "+")
    ))
    
    # Ajustar modelo de regresión logística
    modelo_glm <- stats::glm(formula_glm, data = datos_glm, family = "binomial")
    
    # Resumir resultados del modelo
    resultados <- summary(modelo_glm)$coefficients
    tabla_resultados <- data.frame(
      Variable = rownames(resultados),
      Efecto = ifelse(resultados[, "Estimate"] > 0, "Positivo", "Negativo"),
      Significancia = ifelse(resultados[, "Pr(>|z|)"] < 0.05, "Sí", "No"),
      Coeficiente = round(resultados[, "Estimate"], 2)
    )
    
    # Calcular predicciones y métricas
    predicciones <- predict(modelo_glm, type = "response")
    etiquetas <- datos_glm[[input$variable_dependiente]]
    library(pROC)
    roc <- pROC::roc(etiquetas, predicciones)
    
    # Salidas del modelo
    output$resumen_modelo <- renderText({
      paste(
        "Modelo de Regresión Logística para", input$variable_dependiente, "\n",
        "- Número de observaciones usadas:", nrow(datos_glm), "\n",
        "- AIC del modelo:", round(AIC(modelo_glm), 2), "\n",
        "- Devianza residual:", round(summary(modelo_glm)$deviance, 2)
      )
    })
    
    output$tabla_resultados <- renderTable(tabla_resultados)
    
    output$metricas_modelo <- renderText({
      paste(
        "Precisión del modelo:", round(mean((predicciones > 0.5) == etiquetas) * 100, 2), "%\n",
        "AUC:", round(pROC::auc(roc), 2)
      )
    })
    
    output$curva_roc <- renderPlot({
      pROC::plot.roc(
        roc, col = "blue", 
        main = paste("Curva ROC para", input$variable_dependiente), 
        percent = TRUE, print.auc = TRUE
      )
    })
    
    shiny::showNotification("Modelo de regresión logística ejecutado con éxito.", type = "message")
  })
  
  
}




