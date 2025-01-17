source("Modulos/Funciones/diccionario_respuestas.R")

analisisDescriptivoUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, 
             selectInput(
               ns("categoria_seleccionada"),
               "Seleccione una categoría:",
               choices = NULL,
               selected = NULL
             ),
             checkboxInput(
               ns("incluir_na"),
               "Incluir NA en el análisis",
               value = TRUE
             )  # Nueva opción para incluir/excluir NA
      ),
      column(8,
             uiOutput(ns("checkbox_variables"))
      )
    ),
    fluidRow(
      column(6, 
             tableOutput(ns("tabla_resumen"))
      ),
      column(6, 
             plotOutput(ns("grafico_resumen"), height = "400px")
      )
    ),
    fluidRow(
      column(12,
             tableOutput(ns("tabla_leyenda"))
      )
    )
  )
}



analisisDescriptivo <- function(input, output, session, datos_completos, carpeta_informe, categorias) {
  ns <- session$ns
  
  # Llenar el selectInput con las categorías
  observe({
    req(categorias)
    updateSelectInput(
      session,
      "categoria_seleccionada",
      choices = names(categorias),
      selected = NULL
    )
  })
  
  # Actualizar las variables según la categoría seleccionada
  output$checkbox_variables <- renderUI({
    req(input$categoria_seleccionada, categorias)
    variables <- categorias[[input$categoria_seleccionada]]
    
    nombres_mapeados <- setNames(variables, 
                                 sapply(variables, function(x) dic_titulo_graf[[x]] %||% x))
    
    tagList(
      tags$div(
        style = "column-count: 3; column-gap: 20px;",
        checkboxGroupInput(
          ns("variables_seleccionadas"),
          label = "Seleccione las variables:",
          choices = nombres_mapeados,
          selected = NULL
        )
      ),
      selectInput(
        ns("tipo_grafico"),
        "Tipo de Gráfico:",
        choices = c("Barras Agrupadas", "Barras Apiladas"),
        selected = "Barras Agrupadas"
      )
    )
  })
  
  # Filtrar datos
  datos_filtrados <- reactive({
    req(input$variables_seleccionadas, datos_completos())
    
    variables <- input$variables_seleccionadas
    datos <- datos_completos()
    
    # Verificar que las columnas existan
    if (!all(variables %in% colnames(datos))) {
      showNotification("Algunas variables seleccionadas no están disponibles en los datos.", type = "error")
      return(NULL)
    }
    
    # Seleccionar variables y mantener Municipio
    datos_long <- datos %>%
      select(all_of(variables), Municipio) %>%  # Incluir Municipio en los datos seleccionados
      pivot_longer(cols = all_of(variables), names_to = "Variable", values_to = "Respuesta")
    
    # Aplicar el filtro de NA por variable
    if (!input$incluir_na) {
      datos_long <- datos_long %>%
        filter(!is.na(Respuesta))  # Excluir NAs por variable
    }
    
    if (nrow(datos_long) == 0) {
      showNotification("No hay datos disponibles después del filtrado.", type = "warning")
      return(NULL)
    }
    
    datos_long
  })
  
  # Generar la tabla resumen
  output$tabla_resumen <- renderTable({
    datos <- datos_filtrados()
    req(datos)
    
    # Total de registros en la población
    total_muestra <- nrow(datos_completos())
    
    tabla_resumen <- datos %>%
      group_by(Variable, Respuesta) %>%
      summarise(Conteo = n(), .groups = "drop") %>%
      mutate(
        Porcentaje_Total = round((Conteo / total_muestra) * 100, 1)  # Porcentaje respecto al total de registros
      ) %>%
      arrange(Variable, Respuesta)
    
    if (nrow(tabla_resumen) == 0) {
      showNotification("La tabla resumen está vacía. Verifique los datos seleccionados.", type = "warning")
      return(NULL)
    }
    
    tabla_resumen
  }, rownames = TRUE)
  
  
 # Generar Grafico
  
  output$grafico_resumen <- renderPlot({
    datos <- datos_filtrados()
    req(datos)
    
    total_muestra <- nrow(datos_completos())  # Total de registros en la población
    
    tipo_grafico <- input$tipo_grafico  # Selector del tipo de gráfico
    
    if (tipo_grafico == "Barras Apiladas") {
      # Lógica para barras apiladas
      datos_grafico_apilado <- datos %>%
        group_by(Variable, Respuesta, Municipio) %>%
        summarise(Conteo = n(), .groups = "drop") %>%
        group_by(Variable, Municipio) %>%
        mutate(
          Porcentaje_Local = round((Conteo / sum(Conteo)) * 100, 1)
        ) %>%
        ungroup() %>%
        mutate(
          Variable_Descriptiva = sapply(Variable, function(x) dic_titulo_graf[[x]] %||% x),
          Etiqueta = as.character(Respuesta)
        )  # Sin procesamiento adicional de etiquetas
      
      # Crear el gráfico de barras apiladas
      ggplot(datos_grafico_apilado, aes(x = Municipio, y = Conteo, fill = Etiqueta)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(
          aes(label = paste0(Porcentaje_Local, "%")),  # Mostrar porcentaje local
          position = position_stack(vjust = 0.5)
        ) +
        scale_y_continuous(expand = c(0, 0)) +
        facet_wrap(~ Variable_Descriptiva, scales = "fixed") +  # Escalas fijas
        labs(
          title = "Resumen por Municipio",
          x = "Municipio",
          y = "Conteo",
          fill = "Respuesta"
        ) +
        theme_minimal()
      
    } else if (tipo_grafico == "Barras Agrupadas") {
      # Lógica para barras agrupadas
      datos_grafico_agrupado <- datos %>%
        group_by(Variable, Respuesta) %>%
        summarise(Conteo = n(), .groups = "drop") %>%
        mutate(
          Porcentaje_Total = round((Conteo / total_muestra) * 100, 1),
          Etiqueta = as.character(Respuesta)  # Sin procesamiento adicional de etiquetas
        ) %>%
        mutate(
          Variable_Descriptiva = sapply(Variable, function(x) dic_titulo_graf[[x]] %||% x)
        )
      
      # Crear el gráfico de barras agrupadas
      ggplot(datos_grafico_agrupado, aes(x = Etiqueta, y = Conteo, fill = Variable)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(
          aes(label = paste0(Porcentaje_Total, "%")),  # Mostrar porcentaje total
          position = position_dodge(width = 0.9),
          vjust = -0.5
        ) +
        scale_y_continuous(expand = c(0, 0)) +
        facet_wrap(~ Variable_Descriptiva, scales = "fixed") +  # Escalas fijas
        labs(
          title = "Resumen por Variable",
          x = "Respuesta",
          y = "Conteo",
          fill = "Variable"
        ) +
        theme_minimal()
    }
  })
  
}









