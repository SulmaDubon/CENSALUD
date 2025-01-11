source("Modulos/Funciones/diccionario_respuestas.R")

inferenciaEstadisticaUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .table-wrapper {
        overflow-x: auto; /* Habilitar desplazamiento horizontal */
        -webkit-overflow-scrolling: touch; /* Mejorar experiencia táctil */
      }
    ")),
    fluidRow(
      column(12,
             fluidRow(
               column(4,
                      selectInput(
                        ns("variable_dependiente"),
                        "Seleccione:",
                        choices = NULL
                      )
               ),
               column(4,
                      selectInput(
                        ns("categoria_seleccionada"),
                        "Seleccione Categoría:",
                        choices = NULL
                      )
               ),
               column(4,
                      uiOutput(ns("checkbox_variables"))
               )
             )
      )
    ),
    fluidRow(
      column(4,  # Más estrecho para la tabla de contingencia
             h4("Tabla de Contingencia"),
             h4(textOutput(ns("nombre_variable_dependiente_contingencia"))),
             tags$div(class = "table-wrapper", tableOutput(ns("tabla_contingencia"))),
             actionButton(ns("guardar_contingencia"), "Guardar en Servidor")
      ),
      column(8,  # Más ancho para la tabla de resultados
             h4("Resultados de las Pruebas"),
             h4(textOutput(ns("nombre_variable_dependiente_resultados"))),
             tags$div(class = "table-wrapper", tableOutput(ns("tabla_resultados"))),
             actionButton(ns("guardar_resultados"), "Guardar en Servidor")
      )
    )
  )
}



inferenciaEstadistica <- function(input, output, session, datos_completos, categorias, carpeta_informe) {
  ns <- session$ns
  
  # Variables dependientes y sus leyendas
  variables_leyendas <- list(
    FamDiag = "Miembro de la familia ha sido diagnosticado",
    FamHosp = "Miembro de la familia ha sido hospitalizado",
    Caso_6m = "Caso registrado en los últimos 6 meses",
    ZancViv = "Zancudos observados en el área de vivienda",
    LarvViv = "Larvas observadas en el área de vivienda"
  )
  
  # Mostrar el nombre de la variable dependiente seleccionada
  output$nombre_variable_dependiente_resultados <- renderText({
    req(input$variable_dependiente)
    variables_leyendas[[input$variable_dependiente]]
  })
  
  output$nombre_variable_dependiente_contingencia <- renderText({
    req(input$variable_dependiente)
    variables_leyendas[[input$variable_dependiente]]
  })
  
  
  # Filtrar datos para excluir NA en la variable dependiente
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
  
  # Llenar las opciones de variables dependientes
  observe({
    req(categorias)
    updateSelectInput(
      session,
      "variable_dependiente",
      choices = setNames(names(variables_leyendas), unlist(variables_leyendas))
    )
  })
  
  # Llenar el selector de categorías
  observe({
    req(categorias)
    categorias_filtradas <- categorias[!names(categorias) %in% c("Considerar", "Recolecta", "Familia")]
    categorias_con_municipio <- c("Municipio" = "Municipio", categorias_filtradas)
    updateSelectInput(
      session,
      "categoria_seleccionada",
      choices = names(categorias_con_municipio),
      selected = NULL
    )
  })
  
  # Actualizar las variables según la categoría seleccionada
  output$checkbox_variables <- renderUI({
    req(input$categoria_seleccionada, categorias)
    variables <- if (input$categoria_seleccionada == "Municipio") {
      "Municipio"
    } else {
      categorias[[input$categoria_seleccionada]]
    }
    tags$div(
      style = "column-count: 3; column-gap: 20px;",
      checkboxGroupInput(
        ns("variables_independientes"),
        label = NULL, 
        choices = variables,
        selected = NULL
      )
    )
  })
  
  # Reactiva para la tabla de contingencia
  tabla_contingencia_data <- reactive({
    req(input$variable_dependiente, input$variables_independientes)
    
    datos <- datos_filtrados()
    variable_dep <- input$variable_dependiente
    variable_indep <- input$variables_independientes  # Selección múltiple
    
    # Crear tabla de contingencia con etiquetas descriptivas
    tablas <- lapply(variable_indep, function(var_indep) {
      # Crear tabla de contingencia
      tabla <- table(datos[[var_indep]], datos[[variable_dep]])
      
      # Convertir valores numéricos a etiquetas usando el diccionario_respuestas
      etiquetas_indep <- names(diccionario_respuestas[[var_indep]])[match(rownames(tabla), diccionario_respuestas[[var_indep]])]
      etiquetas_dep <- names(diccionario_respuestas[[variable_dep]])[match(colnames(tabla), diccionario_respuestas[[variable_dep]])]
      
      # Si no hay etiquetas en el diccionario, mantener los valores originales
      if (is.null(etiquetas_indep)) etiquetas_indep <- rownames(tabla)
      if (is.null(etiquetas_dep)) etiquetas_dep <- colnames(tabla)
      
      # Convertir tabla a data.frame con etiquetas
      tabla_df <- as.data.frame(tabla)
      colnames(tabla_df) <- c("Categoría", "Dependiente", "Frecuencia")
      
      # Reestructurar para tener columnas separadas por categoría dependiente
      tabla_wide <- reshape(tabla_df, idvar = "Categoría", timevar = "Dependiente", direction = "wide")
      colnames(tabla_wide) <- c("Categoría", etiquetas_dep)
      tabla_wide <- cbind(Variable = var_indep, tabla_wide)
      
      # Reemplazar las categorías con etiquetas descriptivas
      tabla_wide$Categoría <- etiquetas_indep[match(tabla_wide$Categoría, rownames(tabla))]
      tabla_wide
    })
    
    # Combinar todas las tablas
    do.call(rbind, tablas)
  })
  
  
  # Reactiva para la tabla de resultados
  tabla_resultados_data <- reactive({
    req(input$variable_dependiente, input$variables_independientes)
    
    datos <- datos_filtrados()
    variable_dep <- input$variable_dependiente
    variable_indep <- input$variables_independientes
    
    resultados <- lapply(variable_indep, function(var_indep) {
      tabla_contingencia <- table(datos[[variable_dep]], datos[[var_indep]])
      
      if (ncol(tabla_contingencia) > 2 || nrow(tabla_contingencia) > 2) {
        chi_result <- chisq.test(tabla_contingencia)
        data.frame(
          Variable_Independiente = var_indep,
          Prueba = "Chi-Cuadrado",
          "Valor p" = chi_result$p.value,
          "Estadístico Chi-Cuadrado" = chi_result$statistic,
          "Grados de Libertad" = chi_result$parameter,
          "Odds Ratio" = NA,
          "IC Inferior" = NA,
          "IC Superior" = NA,
          check.names = FALSE
        )
      } else {
        fisher_result <- fisher.test(tabla_contingencia)
        data.frame(
          Variable_Independiente = var_indep,
          Prueba = "Fisher",
          "Valor p" = fisher_result$p.value,
          "Odds Ratio" = fisher_result$estimate,
          "IC Inferior" = fisher_result$conf.int[1],
          "IC Superior" = fisher_result$conf.int[2],
          "Estadístico Chi-Cuadrado" = NA,
          "Grados de Libertad" = NA,
          check.names = FALSE
        )
      }
    })
    
    do.call(rbind, resultados)
  })
  
  # Renderizar las tablas
  output$tabla_contingencia <- renderTable({
    req(tabla_contingencia_data())
    tabla_contingencia_data()
  })
  
  output$tabla_resultados <- renderTable({
    req(tabla_resultados_data())
    tabla_resultados_data()
  })
  
  # Guardar tabla de contingencia en el servidor
  observeEvent(input$guardar_contingencia, {
    req(carpeta_informe, tabla_contingencia_data())
    
    # Obtener los datos de la tabla de contingencia
    contingencia <- tabla_contingencia_data()
    
    # Definir el nombre del archivo
    nombre_archivo <- file.path(carpeta_informe(), paste0("tabla_contingencia_", Sys.Date(), ".xlsx"))
    
    # Guardar el archivo en el servidor
    openxlsx::write.xlsx(contingencia, nombre_archivo)
    
    # Notificar al usuario
    showNotification(paste("Tabla de contingencia guardada en:", nombre_archivo), type = "message")
  })
  
  # Guardar tabla de resultados en el servidor
  observeEvent(input$guardar_resultados, {
    req(carpeta_informe, tabla_resultados_data())
    
    # Obtener los datos de la tabla de resultados
    resultados <- tabla_resultados_data()
    
    # Definir el nombre del archivo
    nombre_archivo <- file.path(carpeta_informe(), paste0("tabla_resultados_", Sys.Date(), ".xlsx"))
    
    # Guardar el archivo en el servidor
    openxlsx::write.xlsx(resultados, nombre_archivo)
    
    # Notificar al usuario
    showNotification(paste("Tabla de resultados guardada en:", nombre_archivo), type = "message")
  })
}






