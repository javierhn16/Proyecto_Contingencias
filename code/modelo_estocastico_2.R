
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

# Función para calcular una anualidad cierta
anualidad <- function(tasa, n) {
  return((1 - (1 + tasa) ^ (-n)) / tasa)
}

# Modelo estocástico ------------------------------------------------------

# Función que permite simular vidas para calcular primas de manera estocástica
realizar_simulaciones <- function(dataframe, num_simulaciones, tasa_rendimiento){
  
  # Obtiene las combinaciones únicas de sexo y edad de los empleados
  combinaciones_unicas <- dataframe %>%
    arrange(edad, sexo) %>% 
    select(edad, sexo) %>%
    distinct()
  
  # Calcula los qx de las combinaciones únicas de empleados
  qx_unicos <- t(obtener_qx_vect(combinaciones_unicas$edad, combinaciones_unicas$sexo))
  
  # Tasas de inflación
  tasa_inflacion <- 0.03 
  
  # Calcula la tasa de interés real y ajusta la tasa de interés nominal por la inflación (ecuación de Fisher)
  tasa_interes_nominal <- (1 + tasa_rendimiento) * (1 + tasa_inflacion) - 1
  
  # Vector de factores de descuento ajustado por la tasa de interés nominal
  v <- (1 + tasa_interes_nominal) ^ -(1:95)
  
  # Vector con el monto de la pension anual más inflación
  tasa_mensual <- (1 + tasa_interes_nominal) ^ (1/12) - 1
  monto_pension_anual <- (300000 * anualidad(tasa_mensual, 12) +
                            30000 * (1 + tasa_mensual) ^ (-12)) * (1 + tasa_inflacion) ^ c(1:95)
  
  # Lista para guardar los resultados
  primas_estocasticas <- list()
  
  for (sim in 1:num_simulaciones) {
    
    # Obtiene los valores aleatorios para llevar a cabo la simulación
    simulaciones <- matrix(runif(90 * 95), nrow = 90, ncol = 95)
    
    # Realiza la simulación
    simulaciones <- qx_unicos[, -c(1, 2)] <= simulaciones
    
    # Encuentra el primer FALSE en cada fila, este indica el año en que la persona muere
    anio_muerte <- apply(simulaciones, 1, function(fila) which(!fila)[1])
    
    # Obtiene la edad en que muere cada persona
    edad_muerte <- combinaciones_unicas$edad + anio_muerte
    
    # Calcula los años hasta la pensión para aquellos que viven hasta la edad de pensión
    anios_hasta_pension <- pmin(65 - qx_unicos[, 'edad'], anio_muerte - 1, na.rm = TRUE)
    
    # Calcula los años que duran pensionados
    anios_pensionados <- ifelse(anio_muerte > (65 - combinaciones_unicas$edad), anio_muerte - (65 - combinaciones_unicas$edad), 0)
    
    # Calcula el valor presente de los pagos de pensiones ajustados por inflación
    suma_vp_pensiones <- sapply(1:length(anios_pensionados), function(i) {
      
      # Calcula el valor presente de la pensión que recibió
      if (!is.na(anios_pensionados[i]) && anios_pensionados[i] > 0) {
        vp_pension <- sum(v[(anios_hasta_pension[i] + 1):(anios_hasta_pension[i] + anios_pensionados[i])] * 
                            monto_pension_anual[(anios_hasta_pension[i] + 1):(anios_hasta_pension[i] + anios_pensionados[i])])
      } else {
        vp_pension <- 0 
      }
      
      # Calcula (en caso de morir) el valor presente del beneficio 
      if (edad_muerte[i] >= 65 && !is.na(edad_muerte[i])) {
        beneficio_vp <- 1000000 * (1 + tasa_interes_nominal) ^ -(anio_muerte[i]) * (1 + tasa_inflacion) ^ (anio_muerte[i])
      } else {
        beneficio_vp <- 5000000 * (1 + tasa_interes_nominal) ^ -(anio_muerte[i]) * (1 + tasa_inflacion) ^ (anio_muerte[i])
      }
      return(vp_pension + beneficio_vp)
    })
    
    # Calcula las anualidades ajustadas por la tasa de interés nominal y la tasa de inflación (ecuación de Fisher)
    suma_vp_anualidades <- sapply(1:length(anios_pensionados), function(i) {
      
      # Si la persona llegó a pensionarse, calcula el valor presente de la anualidad
      if (edad_muerte[i] >= 65 && !is.na(edad_muerte[i])) {
        vp_anualidad <- anualidad(tasa_interes_nominal, anios_hasta_pension[i])
      } else {
        vp_anualidad <- anualidad(tasa_rendimiento, anio_muerte[i])
      }
      return(vp_anualidad)
    })
    
    primas_estocasticas[[sim]] <- suma_vp_pensiones / suma_vp_anualidades
    print(sim) # Indicador del estado de las simulaciones
  }
  
  # Calcular cuartiles 50 y 90 de cada posición en los vectores de la lista
  cuartiles <- sapply(seq_along(primas_estocasticas[[1]]), function(j) {
    valores_j <- sapply(primas_estocasticas, function(x) x[j])
    quantiles <- quantile(valores_j, probs = c(0.50, 0.90), na.rm = TRUE)
    return(quantiles)
  })
  
  return(cuartiles)
}

# Ejecucion del modelo estocástico
# t <- proc.time()
# realizar_simulaciones(base_empleados, 100000, 0.04)
# proc.time() - t

