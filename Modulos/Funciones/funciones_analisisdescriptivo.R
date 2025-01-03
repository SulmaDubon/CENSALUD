

crear_dataframe_recodificado <- function(datos, variables, diccionario_respuestas) {
  datos %>%
    select(all_of(variables)) %>%
    mutate(across(
      everything(),
      ~ if (cur_column() %in% names(diccionario_respuestas)) {
        ifelse(
          is.na(.),
          "No Responde",  # Etiqueta para NA
          recode(
            .,
            !!!as.list(diccionario_respuestas[[cur_column()]]), 
            .default = as.character(.)
          )
        )
      } else {
        .  # Mantener los valores originales si no están en el diccionario
      }
    ))
}
