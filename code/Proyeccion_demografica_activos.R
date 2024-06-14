# Definimos la función que acepta un dataframe `ABC` como argumento
proyeccion_demografica_activos <- function(ABC) {
  
  # Definimos los rangos de edades y años
  edades <- 20:64
  annos <- 2023:2070
  
  # Creamos los dataframes de proyección de muerte para hombres y mujeres
  proyeccion_muerte_activos_Masc <- data.frame(matrix(0, ncol = length(annos), nrow = length(edades) + 1))
  proyeccion_muerte_activos_Fem <- data.frame(matrix(0, ncol = length(annos), nrow = length(edades) + 1))
  
  # Cambiamos los nombres de las columnas y filas
  colnames(proyeccion_muerte_activos_Masc) <- annos
  rownames(proyeccion_muerte_activos_Masc) <- c(edades, "Total")
  
  colnames(proyeccion_muerte_activos_Fem) <- annos
  rownames(proyeccion_muerte_activos_Fem) <- c(edades, "Total")
  
  # Creamos los dataframes de proyección de vivos para hombres y mujeres
  proyeccion_vivos_activos_Masc <- data.frame(matrix(0, ncol = length(annos), nrow = length(edades) + 1))
  proyeccion_vivos_activos_Fem <- data.frame(matrix(0, ncol = length(annos), nrow = length(edades) + 1))
  
  # Cambiamos los nombres de las columnas y filas
  colnames(proyeccion_vivos_activos_Masc) <- annos
  rownames(proyeccion_vivos_activos_Masc) <- c(edades, "Total")
  
  colnames(proyeccion_vivos_activos_Fem) <- annos
  rownames(proyeccion_vivos_activos_Fem) <- c(edades, "Total")
  
  # Proyecciones
  for (j in 1:ncol(proyeccion_muerte_activos_Masc)) {
    for (i in 1:nrow(ABC)) {
      
      if (ABC$Sexo[i] == 1) {
        if (ABC$Edad[i] + j - 1 < 65) {
          if (j == 1) {
            proyeccion_muerte_activos_Masc[as.character(ABC$Edad[i] + j - 1), j] <- 0
            proyeccion_vivos_activos_Masc[as.character(ABC$Edad[i] + j - 1), j] <- proyeccion_vivos_activos_Masc[as.character(ABC$Edad[i] + j - 1), j] + 1
          } else {
            proyeccion_muerte_activos_Masc[as.character(ABC$Edad[i] + j - 1), j] <- (proyeccion_muerte_activos_Masc[as.character(ABC$Edad[i] + j - 1), j] + 1) * (Tablas_SUPEN_Masc[ABC$Edad[i], j + 24])
            proyeccion_vivos_activos_Masc[as.character(ABC$Edad[i] + j - 1), j] <- (proyeccion_vivos_activos_Masc[as.character(ABC$Edad[i] + j - 1), j] + 1) * (1 - Tablas_SUPEN_Masc[ABC$Edad[i], j + 24])
          }
        }
      } else {
        if (ABC$Edad[i] + j - 1 < 65) {
          if (j == 1) {
            proyeccion_muerte_activos_Fem[as.character(ABC$Edad[i] + j - 1), j] <- 0
            proyeccion_vivos_activos_Fem[as.character(ABC$Edad[i] + j - 1), j] <- proyeccion_vivos_activos_Fem[as.character(ABC$Edad[i] + j - 1), j] + 1
          } else {
            proyeccion_muerte_activos_Fem[as.character(ABC$Edad[i] + j - 1), j] <- (proyeccion_muerte_activos_Fem[as.character(ABC$Edad[i] + j - 1), j] + 1) * (Tablas_SUPEN_Fem[ABC$Edad[i], j + 24])
            proyeccion_vivos_activos_Fem[as.character(ABC$Edad[i] + j - 1), j] <- (proyeccion_vivos_activos_Fem[as.character(ABC$Edad[i] + j - 1), j] + 1) * (1 - Tablas_SUPEN_Fem[ABC$Edad[i], j + 24])
          }
        }
      }
      proyeccion_muerte_activos_Masc["Total", j] <- sum(proyeccion_muerte_activos_Masc[1:45, j])
      proyeccion_muerte_activos_Fem["Total", j] <- sum(proyeccion_muerte_activos_Fem[1:45, j])
      
      proyeccion_vivos_activos_Masc["Total", j] <- sum(proyeccion_vivos_activos_Masc[1:45, j])
      proyeccion_vivos_activos_Fem["Total", j] <- sum(proyeccion_vivos_activos_Fem[1:45, j])
    }
  }
  
  # Devolvemos los dataframes de proyección de muerte y vivos
  list(
    proyeccion_mueros_activos_Masc = proyeccion_muerte_activos_Masc,
    proyeccion_muertos_activos_Fem = proyeccion_muerte_activos_Fem,
    proyeccion_vivos_activos_Masc = proyeccion_vivos_activos_Masc,
    proyeccion_vivos_activos_Fem = proyeccion_vivos_activos_Fem
  )
}

# Ejemplo
# tablas_activos <- proyeccion_demografica_activos(ABC)

