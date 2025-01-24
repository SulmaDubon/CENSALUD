mod_tendencias_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Fila para los selectores de Variables y Municipios
    fluidRow(
      # Selector de Variables
      column(
        width = 3, # Ocupa un menor espacio
        checkboxGroupInput(
          inputId = ns("variables_seleccionadas"),
          label = "Seleccione Variables a Graficar:",
          choices = c("Caso_6m", "DENV_1", "DENV_2", "DENV_3", "DENV_4", "ZancViv"),
          selected = "Caso_6m" # Por defecto, una variable seleccionada
        )
      ),
      
      # Selector de Municipios
      column(
        width = 9, # Ocupa el resto del espacio
        checkboxGroupInput(
          inputId = ns("municipios_seleccionados"),
          label = "Seleccione Municipios:",
          choices = NULL # Se actualizará dinámicamente desde el servidor
        )
      )
    ),
    
    # Selector de rango de fechas
    uiOutput(ns("rango_fechas_ui")),
    
    # Espacio dinámico para los gráficos
    uiOutput(ns("graficos_ui"))
  )
}



mod_tendencias_server <- function(input, output, session, datos_completos, carpeta_informe) {
  
  ns <- session$ns
  
  # 1. Cargar municipios dinámicamente
  observe({
    req(datos_completos())
    municipios <- unique(datos_completos()$Municipio)
    
    updateCheckboxGroupInput(
      session,
      inputId = "municipios_seleccionados",
      label = "Seleccione Municipios:",
      choices = municipios,
      selected = municipios[1] # Seleccionar el primer municipio por defecto
    )
  })
  
  # 2. Seleccionar variables dinámicas
  observe({
    req(datos_completos())
    
    variables <- c("Caso_6m", "DENV_1", "DENV_2", "DENV_3", "DENV_4", "ZancViv")
    updateCheckboxGroupInput(
      session,
      inputId = "variables_seleccionadas",
      label = "Seleccione Variables a Graficar:",
      choices = variables,
      selected = variables[1] # Seleccionar la primera variable por defecto
    )
  })
  
  # 3. Rango de fechas dinámico
  output$rango_fechas_ui <- renderUI({
    req(datos_completos())
    
    fechas <- datos_completos()$Fecha
    fechas <- as.Date(fechas, format = "%d/%m/%Y") # Asegurar que las fechas estén en formato Date
    
    sliderInput(
      inputId = ns("rango_fechas"),
      label = "Seleccione el Rango de Fechas:",
      min = min(fechas, na.rm = TRUE),
      max = max(fechas, na.rm = TRUE),
      value = c(min(fechas, na.rm = TRUE), max(fechas, na.rm = TRUE)), # Rango inicial completo
      timeFormat = "%b %Y", # Mostrar mes y año en el deslizador
      dragRange = TRUE # Permitir arrastrar el rango completo
    )
  })
  
  # 4. Filtrar datos según la selección
  datos_filtrados_tendencia <- reactive({
    req(input$municipios_seleccionados, input$variables_seleccionadas, input$rango_fechas, datos_completos())
    
    datos <- datos_completos() %>%
      mutate(
        Fecha = as.Date(Fecha, format = "%d/%m/%Y") # Convertir Fecha a formato Date
      ) %>%
      filter(
        Municipio %in% input$municipios_seleccionados,
        Fecha >= input$rango_fechas[1], # Inicio del rango
        Fecha <= input$rango_fechas[2]  # Fin del rango
      ) %>%
      dplyr::select(Fecha, Municipio, dplyr::all_of(input$variables_seleccionadas))
    
    validate(
      need(nrow(datos) > 0, "No hay datos disponibles para los filtros seleccionados.")
    )
    
    datos
  })
  
  # 5. Preparar datos para el gráfico
  datos_preparados <- reactive({
    datos <- datos_filtrados_tendencia()
    
    datos_long <- datos %>%
      pivot_longer(
        cols = -c(Fecha, Municipio),
        names_to = "variable",
        values_to = "respuesta"
      ) %>%
      filter(!is.na(respuesta)) %>% # Filtrar valores NA
      mutate(
        respuesta = as.integer(respuesta), # Convertir respuestas a enteros
        Fecha = floor_date(Fecha, "month") # Agrupar las fechas al inicio del mes
      ) %>%
      group_by(Fecha, Municipio, variable) %>%
      summarise(
        suma_respuestas = sum(respuesta, na.rm = TRUE), # Sumar respuestas por grupo
        .groups = "drop"
      )
    
    datos_long
  })
  
  
  # 6. Renderizar múltiples gráficos dinámicamente en filas individuales
  output$graficos_ui <- renderUI({
    req(datos_preparados())
    
    municipios <- unique(datos_preparados()$Municipio)
    
    # Crear una lista de gráficos con altura ajustada
    graficos <- lapply(seq_along(municipios), function(i) {
      fluidRow(
        column(
          width = 12, # Cada gráfico ocupa todo el ancho
          plotlyOutput(outputId = ns(paste0("grafico_", i)), height = "200px") # Ajustar la altura
        )
      )
    })
    
    do.call(tagList, graficos) # Combinar todas las filas en un único elemento
  })
  
  
  # 7. Crear gráficos por municipio
  observe({
    req(datos_preparados())
    municipios <- unique(datos_preparados()$Municipio)
    
    for (i in seq_along(municipios)) {
      local({
        index <- i
        output[[paste0("grafico_", index)]] <- renderPlotly({
          datos <- datos_preparados() %>%
            filter(Municipio == municipios[index])
          
          if (nrow(datos) == 0) {
            return(
              ggplotly(
                ggplot() +
                  labs(
                    title = paste("No hay datos disponibles para", municipios[index]),
                    x = "Fecha",
                    y = "Suma de Respuestas"
                  )
              )
            )
          }
          
          p <- ggplot(datos, aes(x = Fecha, y = suma_respuestas, color = variable, group = variable)) +
            geom_line(linewidth = 1) +
            geom_point(size = 2) +
            scale_x_date(
              date_labels = "%b %Y", # Mostrar mes y año
              date_breaks = "1 month" # Intervalos en el eje X
            ) +
            scale_y_continuous(
              limits = c(0, NA), # Asegurar que el eje Y comience en 0
              expand = expansion(mult = c(0, 0.05)) # Pequeño margen superior
            ) +
            labs(
              title = paste("Suma de Respuestas en", municipios[index]),
              x = "Fecha",
              y = "Suma de Respuestas",
              color = "Variable"
            ) +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid.minor = element_blank()
            )
          
          ggplotly(p, tooltip = c("x", "y")) # Personalizar la caja flotante para mostrar solo Fecha y Suma
        })
      })
    }
  })
  
}



