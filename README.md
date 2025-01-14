# Proyecto CENSALUD

Este proyecto es una aplicación Shiny para el análisis, monitoreo y visualización de datos geoespaciales y estadísticos 
relacionados con la vigilancia de enfermedades como el dengue en El Salvador.

## Características Principales

- **Carga de Datos:**
  - Importación de archivos `.csv` y preprocesamiento de datos.
- **Visualización Geoespacial:**
  - Mapas interactivos para el análisis de patrones espaciales.
- **Análisis Descriptivo:**
  - Estadísticas resumidas y visualizaciones.
- **Modelado:**
  - Implementación de modelos de regresión y análisis estadístico avanzado.
- **Tendencias:**
  - Análisis temporal para identificar patrones a lo largo del tiempo.

## Requisitos Previos

- **Sistema Operativo:** Windows, macOS o Linux.
- **Software necesario:**
  - R (versión 4.4.0 o superior).
  - RStudio.
- **Paquetes de R necesarios:**

```r
install.packages(c("htmlwidgets", "shiny", "ggplot2", "plotly", 
                   "fs", "tidyr", "FactoMineR", "DescTools", "readxl", 
                   "leaflet", "purrr", "flextable", "writexl", "kableExtra", 
                   "knitr", "rmarkdown", "broom", "webshot", "tools", 
                   "renv", "tidyverse", "bslib", "DT", "lubridate", "terra", 
                   "MASS", "dplyr"))
```

## Instalación

1. Clona este repositorio en tu máquina local:

```bash
git clone https://github.com/SulmaDubon/CENSALUD.git
```

2. Restaura las dependencias del proyecto usando `renv`:

```r
renv::restore()
```

3. Abre el archivo `app.R` en RStudio.
4. Ejecuta la aplicación con:

```r
shiny::runApp()
```

## Estructura del Proyecto

```plaintext
.
├── app.R
├── CENSALUD.Rproj
├── Modulos/
│   ├── analisis_descriptivo.R
│   ├── analisis_especies.R
│   ├── analisis_geoespacial.R
│   ├── graficos_geo.R
│   ├── inferencia_estadistica.R
│   ├── carga_datos.R
│   ├── visualizacion_geoespacial.R
│   ├── tendencia.R
│   ├── modelado.R
│   └── Funciones/
│       ├── diccionario_respuestas.R
│       ├── funciones_analisisdescriptivo.R
│       ├── funciones_generales.R
│       ├── funciones_cargadatos.R
│       └── funciones_geoespaciales.R
├── www/
│   ├── Barra.png
│   ├── iconos/
│   │   ├── triangulo_rojo.png
│   │   ├── triangulo_verde.png
│   ├── imagen_principal.jpg
│   ├── munual.pdf
│   ├── styles.css
│   ├── variables.html
│   └── variables_files/
└── renv.lock
```

## Contribución

1. Crea una rama para tu contribución:

```bash
git checkout -b nombre-de-tu-rama
```

2. Realiza tus cambios y súbelos al repositorio remoto:

```bash
git add .
git commit -m "Descripción de los cambios"
git push origin nombre-de-tu-rama
```

3. Abre un **Pull Request** en GitHub para revisión.

## Licencia

Este proyecto está bajo la licencia [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/).
