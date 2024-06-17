
#' Función para obtener las probabilidades de muerte anuales para una persona
#' de edad @edad y sexo @sexo.
#' 
#' @param edad (int): Edad de la persona.
#' @param sexo (factor): Sexo de la persona.
#' 
#' @return vector con las probabilidades de muerte
#' 
obtener_qx <- function(edad, sexo){
  
  if (sexo == 0) {
    return(sapply(0:(115 - edad), function(j) as.numeric(qx_masculino[edad + 1 + j,  26 + j])))
  } else {
    return(sapply(0:(115 - edad), function(j) as.numeric(qx_femenino[edad + 1 + j,  26 + j])))
  }
}

#' ---------------------------------------------------------------------------
#' Función para obtener las probabilidades de muerte anuales para un vector de 
#' edades y sexos.
#' 
#' @param data (data_frame): Data_frame con las edades y sexos
#' 
#' @return lista con la edad, sexo y el vector de los qx
#' 
obtener_qx_vect <- function(data) {
  
  # Obtiene los qx de un vector de edades y sexos
  lista_qx <- mapply(obtener_qx, edad = data$edad, sexo = data$sexo, SIMPLIFY = FALSE)
  
  # Guarda los resultados en una lista
  resultados <- lapply(seq_along(data$edad), function(i) {
    list(edad = data$edad[i], 
         sexo = data$sexo[i], 
         qx = lista_qx[[i]])
  })
  
  return(resultados)
}

