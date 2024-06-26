# Función px_acum para calcular el px acumulado usando datos de df_ABC y df_SUPEN
px_acum <- function(df_ABC, df_SUPEN) {
  j <- 1
  probabilidades <- list()
  
  for (i in seq_len(nrow(df_ABC))) {
    x <- df_ABC$Edad[i]
    
    if (i != 1 && df_ABC$Edad[i-1] == x && df_ABC$Sexo[i] == df_ABC$Sexo[i-1]) {
      probabilidades[[j]] <- probabilidades[[j-1]]
    } else {
      año <- 2023
      df <- subset(df_SUPEN, sex == df_ABC$Sexo[i])
      local_px <- list()
      n <- 1
      
      while (any(df$edad == x & df$year == año)) {
        qx <- df[df$edad == x & df$year == año, ]$qx
        local_px[[n]] <- 1 - qx
        n <- n + 1
        x <- x + 1
        año <- año + 1
      }
      
      probabilidades[[j]] <- list(local_px, df_ABC$Sexo[i], df_ABC$Edad[i])
    }
    
    j <- j + 1
  }
  
  return(probabilidades)
}

# Usar la función para obtener las probabilidades de los individuos de la base de datos de la empresa ABC con las probabilidades de la SUPEN
# px <- px_acum(ABC, SUPEN)

# Proyección de la población viva
# Inicializamos un data frame para proyectar luego la población con edades de 0 a 115 años
proy_poblacion <- data.frame(Año = c(2024:2120), poblacion_hombres = numeric(97), poblacion_mujeres = numeric(97))

# Definimos la población inicial de hombres y mujeres 
proy_poblacion$poblacion_hombres[1] = 272
proy_poblacion$poblacion_mujeres[1] = 228

# Función para calcular la población viva en cada año
calcular_poblacion_viva <- function(px, df) {
  acum_hombres <- rep(1, 272)
  acum_mujeres <- rep(1, 228)
  
  for (i in 1:96) {
    m <- 0
    h <- 0
    
    for (j in seq_along(px)) {
      if (px[[j]][[2]] == 1) {
        h <- h + 1
        if (i <= length(px[[j]][[1]])) {
          acum_hombres[h] <- acum_hombres[h] * px[[j]][[1]][[i]]
        }
      } else {
        m <- m + 1
        if (i <= length(px[[j]][[1]])) {
          acum_mujeres[m] <- acum_mujeres[m] * px[[j]][[1]][[i]]
        }
      }
    }
    
    df$poblacion_hombres[i+1] <- sum(acum_hombres)
    df$poblacion_mujeres[i+1] <- sum(acum_mujeres)
  }
  return(df)
}

# proy_poblacion <- calcular_poblacion_viva(px, proy_poblacion)

# Caso población muerta
proy_poblacion_muerta <- proy_poblacion
proy_poblacion_muerta$poblacion_mujeres <- 228 - proy_poblacion$poblacion_mujeres
proy_poblacion_muerta$poblacion_hombres <- 272 - proy_poblacion$poblacion_hombres
# proy_poblacion_muerta

# Proyección para pensionados vivos
proy_pensionados_vivos <- data.frame(Año = c(2024:2120), poblacion_hombres = numeric(97), poblacion_mujeres = numeric(97))

# Función para calcular la población de pensionados vivos
calcular_pensionados_vivos <- function(px, df) {
  acum_hombres <- rep(1, 272)
  acum_mujeres <- rep(1, 228)
  
  for (i in 1:97) {
    m <- 0
    h <- 0
    pensionados_hombres <- numeric(272)
    pensionados_mujeres <- numeric(228)
    
    for (j in seq_along(px)) {
      if (px[[j]][[2]] == 1) {
        h <- h + 1
        if (i <= length(px[[j]][[1]])) {
          if (65 < px[[j]][[3]] + i) {
            pensionados_hombres[h] <- 1
          }
          acum_hombres[h] <- acum_hombres[h] * px[[j]][[1]][[i]]
          pensionados_hombres[h] <- pensionados_hombres[h] * acum_hombres[h]
        }
      } else {
        m <- m + 1
        if (i <= length(px[[j]][[1]])) {
          if (65 < px[[j]][[3]] + i) {
            pensionados_mujeres[m] <- 1
          }
          acum_mujeres[m] <- acum_mujeres[m] * px[[j]][[1]][[i]]
          pensionados_mujeres[m] <- pensionados_mujeres[m] * acum_mujeres[m]
        }
      }
    }
    
    df$poblacion_hombres[i] <- sum(pensionados_hombres)
    df$poblacion_mujeres[i] <- sum(pensionados_mujeres)
    
    if (df$Año[i] > 2119) {
      df$poblacion_hombres[i] <- 0
      df$poblacion_mujeres[i] <- 0
    }
  }
  return(df)
}

# proy_pensionados_vivos <- calcular_pensionados_vivos(px, proy_pensionados_vivos)

# Proyección para pensionados muertos
proy_pensionados_muertos <- data.frame(Año = c(2024:2120), poblacion_hombres = numeric(97), poblacion_mujeres = numeric(97))

# Función para calcular la población de pensionados muertos
calcular_pensionados_muertos <- function(px, df) {
  acum_hombres <- rep(1, 272)
  acum_mujeres <- rep(1, 228)
  
  for (i in 1:97) {
    m <- 0
    h <- 0
    pensionados_hombres <- numeric(272)
    pensionados_mujeres <- numeric(228)
    
    for (j in seq_along(px)) {
      if (px[[j]][[2]] == 1) {
        h <- h + 1
        if (i <= length(px[[j]][[1]])) {
          if (65 < px[[j]][[3]] + i) {
            pensionados_hombres[h] <- 1
          }
          pensionados_hombres[h] <- pensionados_hombres[h] * acum_hombres[h] * (1 - px[[j]][[1]][[i]])
          acum_hombres[h] <- acum_hombres[h] * px[[j]][[1]][[i]]
        }
      } else {
        m <- m + 1
        if (i <= length(px[[j]][[1]])) {
          if (65 < px[[j]][[3]] + i) {
            pensionados_mujeres[m] <- 1
          }
          pensionados_mujeres[m] <- pensionados_mujeres[m] * acum_mujeres[m] * (1 - px[[j]][[1]][[i]])
          acum_mujeres[m] <- acum_mujeres[m] * px[[j]][[1]][[i]]
        }
      }
    }
    
    df$poblacion_hombres[i] <- sum(pensionados_hombres) + (if(i > 1) df$poblacion_hombres[i-1] else 0)
    df$poblacion_mujeres[i] <- sum(pensionados_mujeres) + (if(i > 1) df$poblacion_mujeres[i-1] else 0)
  }
  return(df)
}

