
# Funciones previas -------------------------------------------------------

#' Función que calcula las probabilidades de muerte de una persona de edad @edad
#' y sexo @sexo hasta que cumpla 65 años.
#' 
#' @param edad edad de la persona
#' @param sexo sexo de la persona
#' @returns lista con la edad, sexo y las probabilidad de muerte de la persona
obtener_qx <- function(edad, sexo){
  
  if (sexo == 1) {
    qx <- sapply(0:(64 - edad), function(j) as.numeric(tablas_supen$qx_masculino[edad + 1 + j,  25 + j]))
  } else {
    qx <- sapply(0:(64 - edad), function(j) as.numeric(tablas_supen$qx_femenino[edad + 1 + j,  25 + j]))
  }
  return(list(edad = edad, sexo = sexo, qxs = qx))
}

# Se vectoriza la función obtener_qx
obtener_qx_vect <- Vectorize(obtener_qx)

# Función para realizar simulaciones --------------------------------------
realizar_simulaciones <- function(dataframe, n_simulaciones){
  
  # Crea un vector para guardar los resultados
  resultados <- vector('list', n_simulaciones)
  
  # Obtiene las edades y sexos únicos
  combinaciones_unicas <- dataframe %>%
    select(sexo, edad) %>%
    distinct()
  
  # Obtiene los qx únicos
  qx_unicos <- t(obtener_qx_vect(combinaciones_unicas$edad, combinaciones_unicas$sexo))
  
  # Agrega la probabilidad de muerte a cada empleado
  dataframe <- merge(dataframe, qx_unicos, by = c('edad', 'sexo'), all.x = TRUE) %>%
    select(id, sexo, edad, qxs)
  
  n_row <- nrow(dataframe)
  
  # Realiza las simulaciones
  for (sim in 1:n_simulaciones) {
    
    t <- proc.time() # ---------- Inicia cronómetro
    # Inicialmente todos están vivos
    poblacion <- rep(1, n_row)
    suma_vp <- rep(0, n_row)
    for (anno in 1:46){
      v <- (1.03 / 1.07) ^ (anno - 1)
      for (i in 1:n_row){
        if (length(dataframe$qxs[[i]]) < anno){
          print(i)
          print('muerto por 1')
          poblacion[i] <- 0
        }
        if (poblacion[i] == 1){
          if (runif(1) <= dataframe$qxs[[i]][anno]) {
            print(i)
            print('muerto por 2')
            poblacion[i] <- 0
          }
        }
      }
    }
  }
  print(proc.time() - t) # ---------- Termina cronómetro
  return(dataframe)
}


#t <- proc.time()
(prueba <- realizar_simulaciones(base_empleados, 1))
#proc.time() - t
