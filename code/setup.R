
# Librerías
pacman::p_load(
  rmdformats,
  tidyverse,
  dplyr,
  readxl,
  readr,
  ggplot2,
  plotly,
  lubridate,
  xtable
)

# R scripts
source('code/lectura_datos.R')

source('code/Primas.R')

source('code/Proyeccion_demografica_activos.R')

source('code/proyeccion_financiera.R')

source('code/Proyecciones_demograficas_pensionados.R')

source('code/modelo_estocastico_2.R')