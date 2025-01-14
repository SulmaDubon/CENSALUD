inferenciaEstadisticaUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(".table-wrapper { overflow-x: auto; -webkit-overflow-scrolling: touch; }")),
    fluidRow(
      column(12,
             fluidRow(
               column(6, selectInput(ns("variable_dependiente"), "Seleccione Variable Dependiente:", choices = NULL)),
               column(6, selectInput(ns("categoria_seleccionada"), "Seleccione Categoría:", choices = NULL))
             ),
             fluidRow(
               column(12, uiOutput(ns("checkbox_variables")))
             )
      )
    ),
    tags$hr(style = "border-top: 2px solid #ccc; margin: 20px 0;"),
    fluidRow(
      column(12,
             fluidRow(
               column(3, numericInput(ns("nivel_confianza"), "Confianza (%):", value = 95, min = 80, max = 99, width = "100%")),
               column(3, numericInput(ns("potencia_estadistica"), "Potencia (%):", value = 80, min = 50, max = 99, width = "100%")),
               column(3, numericInput(ns("tamano_efecto"), "Efecto esperado:", value = 0.1, min = 0.01, max = 1, step = 0.01, width = "100%")),
               column(3, checkboxInput(ns("calcular_confianza"), "Incluir tamaño muestral", value = FALSE))
             )
      )
    ),
    fluidRow(
      column(3, 
             h4("Tabla de Contingencia"),
             h4(textOutput(ns("nombre_variable_dependiente_contingencia"))),
             tags$div(class = "table-wrapper", style = "height: 400px; overflow-y: auto;", tableOutput(ns("tabla_contingencia"))),
             downloadButton(ns("descargar_contingencia"), "Descargar Tabla Contingencia")
      ),
      column(9,
             h4("Resultados de las Pruebas"),
             h4(textOutput(ns("nombre_variable_dependiente_resultados"))),
             tags$div(class = "table-wrapper", style = "height: 400px; overflow-y: auto;", tableOutput(ns("tabla_resultados"))),
             downloadButton(ns("descargar_resultados"), "Descargar Tabla Resultados")
      )
    )
  )
}

inferenciaEstadistica <- function(input, output, session, datos_completos, categorias) {
  ns <- session$ns
  
  variables_leyendas <- list(
    FamDiag = "Miembro de la familia ha sido diagnosticado",
    FamHosp = "Miembro de la familia ha sido hospitalizado",
    Caso_6m = "Caso registrado en los últimos 6 meses",
    ZancViv = "Zancudos observados en el área de vivienda",
    LarvViv = "Larvas observadas en el área de vivienda"
  )
  
  output$nombre_variable_dependiente_resultados <- renderText({
    req(input$variable_dependiente)
    variables_leyendas[[input$variable_dependiente]]
  })
  
  output$nombre_variable_dependiente_contingencia <- renderText({
    req(input$variable_dependiente)
    variables_leyendas[[input$variable_dependiente]]
  })
  
  datos_filtrados <- reactive({
    datos <- datos_completos()
    variable_dep <- input$variable_dependiente
    if (is.null(variable_dep)) {
      return(datos)
    }
    datos_filtrados <- datos[!is.na(datos[[variable_dep]]), ]
    if (nrow(datos) != nrow(datos_filtrados)) {
      showNotification(
        paste("Se excluyeron", nrow(datos) - nrow(datos_filtrados), "registros con NA en la variable dependiente."),
        type = "warning"
      )
    }
    datos_filtrados
  })
  
  observe({
    req(categorias)
    updateSelectInput(session, "variable_dependiente", choices = setNames(names(variables_leyendas), unlist(variables_leyendas)))
  })
  
  observe({
    req(categorias)
    categorias_filtradas <- categorias[!names(categorias) %in% c("Considerar", "Recolecta", "Familia")]
    categorias_con_municipio <- c("Municipio" = "Municipio", categorias_filtradas)
    updateSelectInput(session, "categoria_seleccionada", choices = names(categorias_con_municipio), selected = NULL)
  })
  
  output$checkbox_variables <- renderUI({
    req(input$categoria_seleccionada, categorias)
    variables <- if (input$categoria_seleccionada == "Municipio") {
      "Municipio"
    } else {
      categorias[[input$categoria_seleccionada]]
    }
    tags$div(
      style = "column-count: 3; column-gap: 20px;",
      checkboxGroupInput(ns("variables_independientes"), label = "Seleccione Variables Independientes", choices = variables, selected = NULL)
    )
  })
  
  calcular_tamano_muestral <- function(nivel_confianza, potencia, tamano_efecto) {
    alpha <- 1 - (nivel_confianza / 100)
    beta <- 1 - (potencia / 100)
    z_alpha <- qnorm(1 - alpha / 2)
    z_beta <- qnorm(1 - beta)
    n <- ((z_alpha + z_beta)^2 * (0.5 * (1 - 0.5))) / (tamano_efecto^2)
    return(ceiling(n))
  }
  
  tabla_contingencia_data <- reactive({
    req(input$variable_dependiente, input$variables_independientes)
    datos <- datos_filtrados()
    variable_dep <- input$variable_dependiente
    variable_indep <- input$variables_independientes
    tablas <- lapply(variable_indep, function(var_indep) {
      tabla <- table(datos[[var_indep]], datos[[variable_dep]])
      etiquetas_indep <- names(diccionario_respuestas[[var_indep]])[match(rownames(tabla), diccionario_respuestas[[var_indep]])]
      etiquetas_dep <- names(diccionario_respuestas[[variable_dep]])[match(colnames(tabla), diccionario_respuestas[[variable_dep]])]
      if (is.null(etiquetas_indep)) etiquetas_indep <- rownames(tabla)
      if (is.null(etiquetas_dep)) etiquetas_dep <- colnames(tabla)
      tabla_df <- as.data.frame(tabla)
      colnames(tabla_df) <- c("Categoría", "Dependiente", "Frecuencia")
      tabla_wide <- reshape(tabla_df, idvar = "Categoría", timevar = "Dependiente", direction = "wide")
      colnames(tabla_wide) <- c("Categoría", etiquetas_dep)
      tabla_wide <- cbind(Variable = var_indep, tabla_wide)
      tabla_wide$Categoría <- etiquetas_indep[match(tabla_wide$Categoría, rownames(tabla))]
      tabla_wide
    })
    do.call(rbind, tablas)
  })
  
  tabla_resultados_data <- reactive({
    req(input$variable_dependiente, input$variables_independientes)
    datos <- datos_filtrados()
    variable_dep <- input$variable_dependiente
    variable_indep <- input$variables_independientes
    calcular_confianza <- input$calcular_confianza
    nivel_confianza <- input$nivel_confianza
    potencia <- input$potencia_estadistica
    tamano_efecto <- input$tamano_efecto
    
    resultados <- lapply(variable_indep, function(var_indep) {
      tabla_contingencia <- table(datos[[variable_dep]], datos[[var_indep]])
      if (ncol(tabla_contingencia) > 2 || nrow(tabla_contingencia) > 2) {
        chi_result <- chisq.test(tabla_contingencia)
        result <- data.frame(
          Variable_Independiente = var_indep, Prueba = "Chi-Cuadrado", 
          "Valor p" = chi_result$p.value, "Estadístico Chi-Cuadrado" = chi_result$statistic,
          "Grados de Libertad" = chi_result$parameter, "Odds Ratio" = NA, "IC Inferior" = NA, 
          "IC Superior" = NA, check.names = FALSE
        )
        if (calcular_confianza) {
          result$Tamaño_Muestral_Necesario <- calcular_tamano_muestral(nivel_confianza, potencia, tamano_efecto)
        }
        result
      } else {
        fisher_result <- fisher.test(tabla_contingencia, conf.level = nivel_confianza / 100)
        result <- data.frame(
          Variable_Independiente = var_indep, Prueba = "Fisher", "Valor p" = fisher_result$p.value,
          "Odds Ratio" = fisher_result$estimate, "IC Inferior" = fisher_result$conf.int[1],
          "IC Superior" = fisher_result$conf.int[2], "Estadístico Chi-Cuadrado" = NA,
          "Grados de Libertad" = NA, check.names = FALSE
        )
        if (calcular_confianza) {
          result$Tamaño_Muestral_Necesario <- calcular_tamano_muestral(nivel_confianza, potencia, tamano_efecto)
        }
        result
      }
    })
    do.call(rbind, resultados)
  })
  
  output$tabla_contingencia <- renderTable({
    req(tabla_contingencia_data())
    tabla_contingencia_data()
  })
  
  output$tabla_resultados <- renderTable({
    req(tabla_resultados_data())
    tabla_resultados_data()
  })
  
  output$descargar_contingencia <- downloadHandler(
    filename = function() { paste0("tabla_contingencia_", Sys.Date(), ".xlsx") },
    content = function(file) {
      openxlsx::write.xlsx(tabla_contingencia_data(), file)
    }
  )
  
  output$descargar_resultados <- downloadHandler(
    filename = function() { paste0("tabla_resultados_", Sys.Date(), ".xlsx") },
    content = function(file) {
      openxlsx::write.xlsx(tabla_resultados_data(), file)
    }
  )
}


