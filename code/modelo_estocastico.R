
source('code/lectura_datos.R')
source('code/Primas.R')
primas <- Calcula_prima_individuales(base_empleados, tablas_supen, 5000000, 1000000, 300000) %>% 
  select(Empleado, Primas)

# Funciones previas -------------------------------------------------------

#' Función que calcula las probabilidades de muerte de una persona de edad @edad
#' y sexo @sexo hasta que cumpla 65 años.
#'
#' @param sexo sexo de la persona
#' @param edad edad de la persona
#' @returns lista con la edad, sexo y las probabilidad de muerte de la persona
obtener_qx <- function(sexo, edad){
  
  if (sexo == 1) {
    qx <- sapply(0:(64 - edad), function(j) as.numeric(tablas_supen$qx_masculino[edad + 1 + j,  25 + j]))
  } else {
    qx <- sapply(0:(64 - edad), function(j) as.numeric(tablas_supen$qx_femenino[edad + 1 + j,  25 + j]))
  }
  return(list(edad = edad, sexo = sexo, qxs = qx))
}

# Se vectoriza la función obtener_qx
obtener_qx_vect <- Vectorize(obtener_qx)


# Funcón para realizar una simulación -------------------------------------

simular <- function(dataframe, nrow_df, primas) {
  
  # Inicialmente, todos pagan primas ya que no hay pensionados
  no_pensionados <- rep(1, nrow_df)
  suma_vp <- rep(0, nrow_df)
  
  for (anno in 1:46){
    
    # Calcula el factor de descuento de cada año
    v <- (1.03 / 1.07) ^ (anno - 1)
    
    for (i in 1:nrow_df) {
      
      # Si la persona murió o ya se pensionó pasa al siguiente (pues no paga prima)
      if (no_pensionados[i] == 0 | is.na(dataframe$qxs[[i]][anno])){
        next
      }
      
      # Si la persona sigue viva, calcula la prima
      if (no_pensionados[i] == 1) {
        suma_vp[i] <- suma_vp[i] + primas$Primas[i] * v
        
        # Realiza una simulación para el siguiente año
        if (runif(1) < dataframe$qxs[[i]][anno]){
          no_pensionados[i] <- 0
        }
      } 
    }
  }
  return(suma_vp)
}
      

# Función para realizar simulaciones --------------------------------------

realizar_simulaciones <- function(dataframe, n_simulaciones){
  
  # Obtiene combinaciones únicas de sexo y edad
  combinaciones_unicas <- dataframe %>%
    select(sexo, edad) %>%
    distinct()
  
  # Calcula qx únicos
  qx_unicos <- t(obtener_qx_vect(combinaciones_unicas$sexo, combinaciones_unicas$edad))
  
  # Agrega los qx a cada empleado
  dataframe <- merge(dataframe, qx_unicos, by = c('edad', 'sexo'), all.x = TRUE) %>%
    select(id, sexo, edad, qxs) %>% 
    arrange(id)
  
  # Obtiene el número de empleados
  nrow_df <- nrow(dataframe)
  
  # Ejecuta las simulaciones usando lapply
  resultados <- lapply(1:n_simulaciones, function(x) simular(dataframe, nrow_df, primas))
  
  # Convierte la lista de resultados en una matriz
  resultados <- do.call(rbind, resultados)
  
  return(resultados)
}


# Cálculo de percentiles 50 y 90 ------------------------------------------

calcular_percentiles <- function(resultados, n_simulaciones) {
  
  # Calcula valores presentes y percentiles
  valores_presentes <- colSums(resultados) / n_simulaciones
  percentil_50 <- quantile(valores_presentes, 0.5)
  percentil_90 <- quantile(valores_presentes, 0.9)

  return(list(
    promedios = valores_presentes,
    primas_percentil_50 = percentil_50,
    primas_percentil_90 = percentil_90
    ))
}


# Ejecución del modelo estocástico ----------------------------------------

# Número de simulaciones a realizar
n_simulaciones <- 50000

# Realiza las simulaciones
t <- proc.time()
resultados <- realizar_simulaciones(base_empleados, n_simulaciones)
proc.time() - t

# Guarda las simulaciones en un .csv
write.csv(resultados, "docs/resultados_estocasticos.csv", row.names = FALSE)

# Calcula los promedios y los percentiles
percentiles <- calcular_percentiles(resultados, n_simulaciones)

# Guarda los promedios
write.csv(percentiles$promedios, "docs/promedio_primas_estocasticas.csv", row.names = FALSE)

# Guarda los percentiles
write.csv(percentiles$primas_percentil_50, "docs/percentil_50.csv", row.names = FALSE)
write.csv(percentiles$primas_percentil_90, "docs/percentil_90.csv", row.names = FALSE)

# Genera histograma para visualizar los resultados
hist(log(percentiles$promedios), 
     breaks = 15, 
     main = "Histograma de las primas estocásticas", 
     xlab = "Primas", 
     ylab = "Frecuencia", 
     col = "lightgreen", 
     border = "darkgreen")
