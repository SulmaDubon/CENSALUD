inferenciaEstadisticaUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             selectInput(
               ns("variable_dependiente"),
               "Seleccione Variable Dependiente:",
               choices = NULL
             ),
             selectInput(
               ns("categoria_seleccionada"),
               "Seleccione Categoría de Variables Independientes:",
               choices = NULL
             ),
             uiOutput(ns("checkbox_variables"))  # Variables independientes dinámicas
      ),
      column(8,
             navlistPanel(
               tabPanel("Prueba de Fisher", 
                        tableOutput(ns("fisher_resultados")),
                        actionButton(ns("agregar_a_lista"), "Agregar a Lista") # Botón para agregar a la lista
               ),
               tabPanel("Intervalos de Confianza", tableOutput(ns("intervalos_resultados")))
             ),
             
             # Tabla para mostrar los resultados
             tableOutput(ns("tabla_resultados")), 
             
             # Botón para guardar la tabla
             downloadButton(ns("guardar_tabla"), "Guardar Tabla") 
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
    categorias_con_municipio <- c("Municipio" = "Municipio", categorias)
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
  
  # Variable reactiva para almacenar los resultados
  resultados_lista <- reactiveVal(data.frame())
  
  # Lógica para Fisher (modificada)
  output$fisher_resultados <- renderTable({
    req(input$variable_dependiente, input$variables_independientes, datos_completos())
    
    datos <- datos_completos()
    variable_dep <- input$variable_dependiente
    variable_indep <- input$variables_independientes
    
    if (length(variable_indep) > 0) {
      fisher_tabla <- table(datos[[variable_dep]], datos[[variable_indep[1]]])
      fisher_result <- fisher.test(fisher_tabla)
      
      # Usamos broom para convertir la salida a un data frame
      fisher_tidy <- broom::tidy(fisher_result)
      
      # Creamos una tabla con los resultados (modificada)
      resultados <- data.frame(
        Prueba = "Prueba exacta de Fisher",
        "Tabla de Contingencia" = paste(capture.output(fisher_tabla), collapse = "\n"),
        "Valor p" = fisher_tidy$p.value,
        "Odds ratio" = fisher_tidy$estimate,
        "IC Inferior" = fisher_tidy$conf.low,
        "IC Superior" = fisher_tidy$conf.high,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      resultados
    } else {
      NULL
    }
  })
  
  # Función para agregar resultados a la lista
  observeEvent(input$agregar_a_lista, {
    req(input$variable_dependiente, input$variables_independientes, datos_completos())
    
    datos <- datos_completos()
    variable_dep <- input$variable_dependiente
    variable_indep <- input$variables_independientes
    
    if (length(variable_indep) > 0) {
      fisher_tabla <- table(datos[[variable_dep]], datos[[variable_indep[1]]])
      fisher_result <- fisher.test(fisher_tabla)
      fisher_tidy <- broom::tidy(fisher_result)
      
      # Obtener la leyenda de la variable dependiente
      leyenda_dep <- variables_leyendas[[variable_dep]]
      
      # Crear un data frame con los resultados
      nuevo_resultado <- data.frame(
        "Variable Dependiente" = leyenda_dep,
        "Variable Independiente" = variable_indep[1],
        "Valor p" = fisher_tidy$p.value,
        "Significativo" = ifelse(fisher_tidy$p.value < 0.05, "Sí", "No"),
        "Odds Ratio" = fisher_tidy$estimate
      )
      
      # Agregar el nuevo resultado a la lista
      resultados_lista(rbind(resultados_lista(), nuevo_resultado))
    }
  })
  
  # Mostrar la tabla de resultados
  output$tabla_resultados <- renderTable({
    req(nrow(resultados_lista()) > 0)
    
    # Agregar encabezado a la tabla
    tabla_final <- resultados_lista()
    colnames(tabla_final) <- c("Variable Dependiente", "Variable Independiente", "Valor p", "Significativo", "Odds Ratio")
    
    tabla_final
  })
  
  # Guardar la tabla en la carpeta "informe"
  output$guardar_tabla <- downloadHandler(
    filename = function() {
      paste("resultados_fisher", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Construir la ruta completa del archivo
      ruta_archivo <- file.path(carpeta_informe(), file)
      
      # Imprimir la ruta del archivo en la consola para verificar
      print(paste("Guardando la tabla en:", ruta_archivo))
      
      # Intentar crear la carpeta "informe" si no existe
      if (!dir.exists(carpeta_informe())) {
        dir.create(carpeta_informe())
        print("Carpeta 'informe' creada.")
      }
      
      # Guardar la tabla en la ruta especificada
      write.csv(resultados_lista(), ruta_archivo, row.names = FALSE)
      print(paste("Tabla guardada en:", ruta_archivo))
    }
  )
  
  # Función para calcular el tamaño de la muestra
  calcular_tamaño_muestra <- function(p) {
    z <- qnorm(0.975)  # Valor Z para un nivel de confianza del 95%
    e <- 0.05  # Margen de error del 5%
    n <- (z^2 * p * (1 - p)) / e^2
    return(ceiling(n))  # Redondear hacia arriba
  }
  
  # Lógica para Intervalos de Confianza 
  output$intervalos_resultados <- renderTable({
    req(input$variable_dependiente, datos_completos())
    
    datos <- datos_completos()
    variable_dep <- input$variable_dependiente
    
    # Cálculo de proporciones e intervalos de confianza
    tabla <- datos %>%
      group_by(!!sym(variable_dep)) %>%
      summarise(
        n = n(),
        p = n() / nrow(datos),
        .groups = "drop"
      ) %>%
      mutate(
        IC_Lower = p - qnorm(0.975) * sqrt((p * (1 - p)) / nrow(datos)),
        IC_Upper = p + qnorm(0.975) * sqrt((p * (1 - p)) / nrow(datos))
      )
    
    # Calcular el tamaño de la muestra
    tabla <- tabla %>%
      rowwise() %>%
      mutate(Tamaño_muestra = calcular_tamaño_muestra(p))
    
    # Agregar la leyenda de la variable dependiente
    tabla <- tabla %>%
      mutate("Variable Dependiente" = variables_leyendas[[variable_dep]]) %>%
      select("Variable Dependiente", everything())
    
    # Agregar título a la tabla
    tabla_final <- tabla
    colnames(tabla_final) <- c("Variable Dependiente", "Categoría", "n", "p", "IC Inferior", "IC Superior", "Tamaño de muestra")
    
    tabla_final
  })
}


