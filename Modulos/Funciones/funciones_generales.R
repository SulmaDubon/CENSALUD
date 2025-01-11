# Modulos/Funciones/funciones_generales

# Función para instalar y cargar librerías necesarias
instalar_cargar_librerias <- function(librerias) {
  for (libreria in librerias) {
    if (!require(libreria, character.only = TRUE)) {
      install.packages(libreria, dependencies = TRUE)
      library(libreria, character.only = TRUE)
    }
  }
}

# Lista de librerías necesarias
librerias_necesarias <- c(
  "htmlwidgets", "shiny", "dplyr", "ggplot2", "plotly", 
  "fs", "tidyr", "FactoMineR", "DescTools", "readxl", 
  "leaflet", "purrr", "flextable", "writexl", "kableExtra",
  "knitr", "rmarkdown", "broom", "webshot", "tools", 
  "renv", "tidyverse", "bslib", "DT"
)


# Instalar y cargar las librerías
instalar_cargar_librerias(librerias_necesarias)

install.packages("terra")

if (!webshot::is_phantomjs_installed()) {
  webshot::install_phantomjs()
}

install.packages("webshot")

