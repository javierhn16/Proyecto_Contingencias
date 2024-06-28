
source('code/lectura_datos.R') # borrar esto al final

# Funciones previas -------------------------------------------------------

#' Función que calcula las probabilidades de muerte de una persona de edad @edad
#' y sexo @sexo hasta que cumpla 65 años.
#'
#' @param sexo sexo de la persona
#' @param edad edad de la persona
#' @returns lista con la edad, sexo y las probabilidad de muerte de la persona
obtener_qx <- function(edad, sexo) {
  
  if (sexo == 1) {
    qx <- sapply(0:(114 - edad), function(j) as.numeric(tablas_supen$qx_masculino[edad + 1 + j,  25 + j]))
  } else {
    qx <- sapply(0:(114 - edad), function(j) as.numeric(tablas_supen$qx_femenino[edad + 1 + j,  25 + j]))
  }
  
  if (length(qx) < 95) {
    qx <- c(qx, rep(NA, (95 - length(qx))))
  }
  
  return(setNames(cbind(sexo, edad, t(qx)), c("sexo", "edad", paste0("qx", seq_along(qx)))))
}

# Se vectoriza la función obtener_qx
obtener_qx_vect <- Vectorize(obtener_qx)




# Modelo estocástico ------------------------------------------------------

# Obtiene las combinaciones únicas de sexo y edad de los empleados
combinaciones_unicas <- base_empleados %>%
  select(edad, sexo) %>%
  distinct()

# Calcula los qx de las combinaciones únicas de empleados
qx_unicos <- t(obtener_qx_vect(combinaciones_unicas$edad, combinaciones_unicas$sexo))

# Tasas de rendimiento real e inflación
tasa_rendimiento_real <- 0.04
tasa_inflacion <- 0.03

# Calcula la tasa de interés real y ajusta la tasa de interés nominal por la inflación (ecuación de Fisher)
tasa_interes_real <- tasa_rendimiento_real - tasa_inflacion
tasa_interes_nominal <- (1 + tasa_interes_real) * (1 + tasa_inflacion) - 1

# Vector de factores de descuento ajustado por la tasa de interés nominal
v <- (1 + tasa_interes_nominal) ^ (1:95)


# Simulación --------------------------------------------------------------

# Obtiene los valores aleatorios para llevar a cabo la simulación
simulaciones <- matrix(runif(90 * 95), nrow = 90, ncol = 95)

# Realiza la simulación
simulaciones <- qx_unicos[, -c(1, 2)] <= simulaciones

# Encuentra el primer FALSE en cada fila, este indica el año en que la persona muere
anio_muerte <- apply(simulaciones, 1, function(fila) which(!fila)[1])

# Calcula los años hasta la pensión para aquellos que viven hasta la edad de pensión
anios_hasta_pension <- pmin(65 - qx_unicos[, 'edad'], anio_muerte - 1, na.rm = TRUE)

# Calcula los años que duran pensionados
anios_pensionados <- ifelse(anio_muerte > (65 - combinaciones_unicas$edad), anio_muerte - (65 - combinaciones_unicas$edad), 0)

# Calcular el valor presente de los pagos de pensiones con sapply
suma_vp_pensiones <- sapply(1:length(anios_pensionados), function(i) {
  if (anios_pensionados[i] > 0) {
    return(sum(v[1:anios_pensionados[i]]))
  } else {
    return(0)
  }
})

# Calcula las anualidades ajustadas por la tasa de interés nominal y la tasa de inflación (ecuación de Fisher)
suma_vp_anualidades <- sapply(combinaciones_unicas$edad, function(edad) sum((1 / (1 + tasa_interes_nominal)^(1:(65 - edad)))))

# Calcula las primas
primas <- suma_vp_pensiones / suma_vp_anualidades

print(primas)













