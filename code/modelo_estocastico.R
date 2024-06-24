
source('code/lectura_datos.R')

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

simular <- function(dataframe, nrow_df){
  
  poblacion <- rep(1, nrow_df)
  suma_vp <- rep(0, nrow_df)
  
  for (anno in 1:46){
    # Calcula el factor de descuento de cada año
    v <- (1.03 / 1.07) ^ (anno - 1)
    
    sapply(1:nrow_df, function(i){
      if (poblacion[i] == 1 && length(dataframe$qxs[[i]]) >= anno && runif(1) < dataframe$qxs[[i]][anno]) {
        poblacion[i] <- 0
        suma_vp[i] <- suma_vp[i] + 5000000 * v
      }
    })
  }
  return(suma_vp)
}


# Función para realizar simulaciones --------------------------------------

realizar_simulaciones <- function(dataframe, n_simulaciones){
  
  # Inicializa resultados
  resultados <- matrix(0, nrow = n_simulaciones, ncol = nrow(dataframe))
  
  # Obtiene combinaciones únicas de sexo y edad
  combinaciones_unicas <- dataframe %>%
    select(sexo, edad) %>%
    distinct()
  
  # Calcula qx únicos
  qx_unicos <- t(obtener_qx_vect(combinaciones_unicas$sexo, combinaciones_unicas$edad))
  
  # Agrega los qx a cada empleado
  dataframe <- merge(dataframe, qx_unicos, by = c('edad', 'sexo'), all.x = TRUE) %>%
    select(id, sexo, edad, qxs)
  
  # Obtiene el número de empleados
  nrow_df <- nrow(dataframe)
  
  # Ejecuta las simulaciones en paralelo usando lapply
  resultados <- lapply(1:n_simulaciones, function(x) simular(dataframe, nrow_df))
  
  # Convierte la lista de resultados en una matriz
  resultados <- do.call(rbind, resultados)
  
  return(resultados)
}


# Ejecución del modelo estocástico ----------------------------------------

# Número de simulaciones a realizar
n_simulaciones <- 1

# Realiza las simulaciones
t <- proc.time()
resultados <- realizar_simulaciones(base_empleados, n_simulaciones)
proc.time() - t

# Calcula valores presentes y percentiles
valores_presentes <- rowSums(resultados)
percentil_50 <- quantile(valores_presentes, 0.50)
percentil_90 <- quantile(valores_presentes, 0.90)

# Calcula prima anual nivelada ajustada por inflación
prima_anual_50 <- percentil_50 / sum((1.03) ^ (0:45))
prima_anual_90 <- percentil_90 / sum((1.03) ^ (0:45))

# Resultados
percentiles <- list(
  prima_anual_50 = prima_anual_50,
  prima_anual_90 = prima_anual_90
)
