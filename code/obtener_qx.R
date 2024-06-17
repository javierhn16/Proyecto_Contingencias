
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
    return(sapply(0:(115 - edad), function(j) qx_masculino[edad + 1 + j,  26 + j]))
  } else {
    return(sapply(0:(115 - edad), function(j) qx_femenino[edad + 1 + j,  26 + j]))
  }
}