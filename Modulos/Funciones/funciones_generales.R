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
  "leaflet"
)

install.packages("purrr")
library(tidyverse)


# Instalar y cargar las librerías
instalar_cargar_librerias(librerias_necesarias)