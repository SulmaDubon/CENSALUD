# Función para instalar y cargar librerías
instalar_cargar_librerias <- function(librerias) {
  for (libreria in librerias) {
    if (!require(libreria, character.only = TRUE)) {
      cat(paste("Instalando librería:", libreria, "\n"))
      install.packages(libreria, dependencies = TRUE)
      library(libreria, character.only = TRUE)
    }
  }
}

# Lista de librerías necesarias
librerias_necesarias <- c(
  "htmlwidgets", "shiny", "ggplot2", "plotly", 
  "fs", "tidyr", "FactoMineR", "DescTools", "readxl", 
  "leaflet", "purrr", "flextable", "writexl", "kableExtra",
  "knitr", "rmarkdown", "broom", "webshot", "tools", 
  "renv", "tidyverse", "bslib", "DT", "lubridate", "terra", 
  "MASS", "dplyr"
)

# Instalar y cargar las librerías
instalar_cargar_librerias(librerias_necesarias)

# Verificar e instalar PhantomJS
if (!requireNamespace("webshot", quietly = TRUE)) {
  install.packages("webshot", dependencies = TRUE)
  library(webshot)
}

if (!webshot::is_phantomjs_installed()) {
  cat("Instalando PhantomJS...\n")
  webshot::install_phantomjs()
} else {
  cat("PhantomJS ya está instalado.\n")
}

