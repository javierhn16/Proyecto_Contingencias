
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
  tasa_mensual <- (1 + tasa_rendimiento) ^ (1/12) - 1
  monto_pension_anual <- (300000 * anualidad(tasa_mensual, 12) + 
                            300000 * (1 + tasa_mensual) ^ (-12)) * (1 + tasa_inflacion) ^ c(1:95)
  
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
        vp_pension <- sum(v[anios_hasta_pension[i]:(anios_hasta_pension[i] + anios_pensionados[i])] * 
                            monto_pension_anual[anios_hasta_pension[i]:(anios_hasta_pension[i] + anios_pensionados[i])])
      } else {
        vp_pension <- 0 
      }
      
      # Calcula (en caso de morir) el valor presente del beneficio 
      if (edad_muerte[i] >= 65) {
        beneficio_vp <- 1000000 * (1 + tasa_interes_nominal) ^ -(anio_muerte[i])
      } else {
        beneficio_vp <- 0
      }
      return(vp_pension + beneficio_vp)
    })
    
    # Calcula las anualidades ajustadas por la tasa de interés nominal y la tasa de inflación (ecuación de Fisher)
    suma_vp_anualidades <- sapply(1:length(anios_pensionados), function(i) {
      
      # Si la persona llegó a pensionarse, calcula el valor presente de la anualidad
      if (edad_muerte[i] >= 65) {
        vp_anualidad <- anualidad(tasa_rendimiento, anios_hasta_pension[i])
        primas_estocasticas[[sim]] <<- suma_vp_pensiones / vp_anualidad
      } else {
        primas_estocasticas[[sim]] <<- (5000000 * (1 + tasa_interes_nominal) ^ -(anio_muerte[i])) / 
          anualidad(tasa_rendimiento, anio_muerte[i])
      }
    })
  
    print(sim) ################### BORRAR!!!!!!!
  }
  
  # Calcula  el promedio de las primas por entrada
  promedio_primas <- sapply(seq_along(primas_estocasticas[[1]]), function(j) {
    mean(sapply(primas_estocasticas, function(prima_sim) prima_sim[j]))
  })
  
  return(list('ano muerte' = anio_muerte, 'hasta pension' = anios_hasta_pension, 'anos pensionados' = anios_pensionados, primas = primas_estocasticas))
}


# Calculo de cuartiles ----------------------------------------------------

calcular_cuartiles <- function(vector_primas) {
  
  # Calcula la mediana (cuartil 50)
  cuartil_50 <- median(vector_primas)
  
  # Calcula el cuartil 90
  cuartil_90 <- quantile(vector_primas, probs = 0.9)
  
  # Devolver los resultados como un vector
  return(list('Cuartil 50' = cuartil_50, 'Cuartil 90' = cuartil_90))
}


t <- proc.time()
realizar_simulaciones(base_empleados, 3, 0.04)
proc.time() - t

