
# Funciones previas -------------------------------------------------------

#' Función que calcula las probabilidades de muerte de una persona de edad @edad
#' y sexo @sexo hasta que cumpla 65 años.
#' 
#' @param edad edad de la persona
#' @param sexo sexo de la persona
#' @returns lista con la edad, sexo y las probabilidad de muerte de la persona
obtener_qx <- function(edad, sexo){
  
  if (sexo == 0) {
    qx <- sapply(0:(64 - edad), function(j) as.numeric(qx_masculino[edad + 1 + j,  25 + j]))
  } else {
    qx <- sapply(0:(64 - edad), function(j) as.numeric(qx_femenino[edad + 1 + j,  25 + j]))
  }
  return(list(edad = edad, sexo = sexo, probabilidad_muerte = qx))
}

# Se vectoriza la función obtener_qx
obtener_qx_vect <- Vectorize(obtener_qx)


# Función para realizar simulaciones --------------------------------------
realizar_simulaciones <- function(data, n_simulaciones){
  
  # Crea un vector para guardar los resultados
  resultados <- vector('list', n_simulaciones)
  
  # Obtiene las edades y sexos únicos
  combinaciones_unicas <- data %>%
    select(sexo, edad) %>%
    distinct()
  
  # Obtiene los qx únicos
  qx_unicos <- t(obtener_qx_vect(combinaciones_unicas$edad, combinaciones_unicas$sexo))
  
  # Agrega la probabilidad de muerte a cada empleado
  data <- merge(data, qx_unicos, by = c('edad', 'sexo'), all.x = TRUE)
  
  # for (sim in seq_len(n_simulaciones)) {
  #   
  #   # Crea dataframe para la simulación
  #   simulacion <- data %>%
  #     select(id, edad, sexo) %>%
  #     mutate(vivo = 1)  # Inicialmente, todos están vivos
  #   
  # }
}


t <- proc.time()
prueba <- realizar_simulaciones(base_empleados, 1)
proc.time() - t




