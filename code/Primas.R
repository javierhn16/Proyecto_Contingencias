
# Descuenta los pagos mensuales de la pensión un año para tener la anualidad anual

descuento_anual <- function(cantidad){
  
  # bajo el supuesto de que 0.04 es la tasa convertible mensualmente
  i <- 0.04/12
  v <- 1/(1 + i)
  m <- 0:13
  suma <- cantidad * sum(v^m)
  return(suma)
}

#Función para tomar las combinaciones únicas de edad y sexo
unico <- function(data){
  combinaciones_unicas <- data %>%
    group_by(sexo, edad) %>%
    slice(1) %>%  # Seleccionar la primera fila de cada grupo (sexo, edad)
    ungroup() %>%
    select(sexo, edad, id)
}

# Función de crecimiento geométrico para la inflación de las primas

# x: Edad en que se asegura
# s: sexo, 1 masculino, 2 femenino
# lista: tabla de mortalidad 

Ia_Geometrica <- function(x, s, lista){
  #Dado que todo está en terminos anuales, se puede usar la i así
  i <- 0.07
  #El v para un año
  v <- 1/(1 + i)
  # m: Tiempo máximo de pago, dado que se retiran a los 65 
  m <- 65 - x
  suma <- 0
  for (t in 1:m){
    suma <- suma + (v^(t)) * (1.03)^(t-1) * as.double(1 - lista[[s]][x + t - 1, 25 + t - 1])
  }
  return(suma)
}

# Función para calcular la esperanza de los beneficios 

# x: Edad en que se asegura
# s: sexo, 1 masculino, 2 femenino
# lista: tabla de mortalidad 
# suma_asegurada_activo: Beneficio en caso de ser empleado activo primer año
# suma_asegurada_pensionado: Beneficio en caso de ser pensionado primer año
# pension_mensual: Beneficio de la pensión mensual primer año

valor_presente_beneficios <- function(x, s, lista,suma_asegurada_activo,suma_asegurada_pensionado,pension_mensual) {
  # tasa anual
  i <- 0.07
  # Valor Presente en caso de fallecimiento 
  VPF <- 0  
  # Valor Presente en caso de sobrevivencia (pago pensión)
  VPS <- 0
  # Auxiliar para obtener probabilidad de sobrevivencia acumulada
  p_x <- 1
  # Auxiliar para conteo del tiempo después de 65 años
  t_p <- 0
  #para solo un año y aginaldo
  v <- 1/(1 + i)
  m <- 0:13
  
  
  # Pensión anualizada
  anualidad_pensión <- descuento_anual(pension_mensual)
  
  for (t in 1:(nrow(lista[[s]]) - x)) {
    #Probabilidad de muerte en un año específico
    q_t <- as.double(lista[[s]][x + t - 1, 25 + t - 1])
    #El v para la cantidad de años transcurridos
    v_t <- (1 / (1 + i))^t
    #Probabilidad de sobrevivencia acumulada
    p_x <- p_x * (1 - q_t)
    
    #Si la persona es menor de 65 años, lo cubre seguro temporal
    if (x + t - 1 < 65) {
      
      VPF <- VPF + suma_asegurada_activo * (1.03)^(t - 1) * v_t * p_x * q_t
      
      
    } #Al sobrepasar los 65 años, tiene seguro vitalicio y pensión vitalicia
    else if (x + t - 1 >= 65) {
      
      VPF <- VPF + suma_asegurada_pensionado * (1.03)^t_p * v_t * p_x * q_t 
      VPS <- VPS + anualidad_pensión * (1.03)^(t - 1) * v_t * p_x
      t_p <- t_p + 1
      
    }
  }
  
  VP <- VPS + VPF
  return(VP)
}

# Función para calculo de primas

#Base : Base de empleados 
#Tabla_mortal : Lista de las tabla de mortalidad separada por sexo: 1 (hombre), 2 (mujer)
#suma_asegurada_activo: Suma del seguro en caso de ser empleado activo y fallecer
#suma_asegurada_pensionado: Suma de seguro en caso de ser pensionadoi y fallecer 
#pension_mensual: Plan de pensión mensual


Calcula_prima_individuales <- function (Base, Tabla_mortal,suma_asegurada_activo,suma_asegurada_pensionado,pension_mensual){
  # Crecimiento de la prima para cada empleado 
  
  tabla_resultados <- data.frame(anualidad = numeric(nrow(Base)))
  
  for (i in 1:nrow(Base)){
    if (Base$sexo[i] == 1){
      if (Base$edad[i] == Base$edad[i-1] && (i-1) > 0 && Base$sexo[i-1] == 1){
        tabla_resultados$anualidad[i] <- tabla_resultados$anualidad[i-1]
      }else {
        tabla_resultados$anualidad[i] <- Ia_Geometrica(Base$edad[i], 1, Tabla_mortal)
      }
    } else if (Base$sexo[i] == 2){
      if (Base$edad[i] == Base$edad[i-1] && (i-1) > 0 && Base$sexo[i-1] == 2) {
        tabla_resultados$anualidad[i] <- tabla_resultados$anualidad[i-1]
      }else{
        tabla_resultados$anualidad[i] <- Ia_Geometrica(Base$edad[i], 2, Tabla_mortal)
      }
    }
  }
  
  # Esperanza de los Beneficios
  
  tabla_resultados2 <- data.frame(beneficios = numeric(nrow(Base)))
  
  for (i in 1:nrow(Base)){
    if (Base$sexo[i] == 1){
      if (Base$edad[i] == Base$edad[i-1] && (i-1) > 0 && Base$sexo[i-1] == 1){
        tabla_resultados2$beneficios[i] <- tabla_resultados2$beneficios[i-1]
      }else{
        tabla_resultados2$beneficios[i] <- valor_presente_beneficios(Base$edad[i], 1, Tabla_mortal,suma_asegurada_activo,suma_asegurada_pensionado,pension_mensual) 
      }
    } else if (Base$sexo[i] == 2){
      if (Base$edad[i] == Base$edad[i-1] && (i-1) > 0 && Base$sexo[i-1] == 2){
        tabla_resultados2$beneficios[i] <- tabla_resultados2$beneficios[i-1]
      }else{
        tabla_resultados2$beneficios[i] <- valor_presente_beneficios(Base$edad[i], 2, Tabla_mortal,suma_asegurada_activo,suma_asegurada_pensionado,pension_mensual)
      }
    }
  }
  
  # Resultados de primas 
  
  Primas_individuales <- data.frame(Empleado = Base$id,
                                    Sexo = Base$sexo,
                                    Edad = Base$edad,
                                    Primas = tabla_resultados2$beneficios / tabla_resultados$anualidad,
                                    vp = tabla_resultados,
                                    beneficios = tabla_resultados2)
  
  return(Primas_individuales)
}



#Proyección financiera de beneficios de muerte para pensionados 
#proy_pensionados_muertos: Es la proyección de pensionados que mueren
#suma_asegurada_pensionados: Es la suma que se asegura para los pensionados en el contrato

Proyeccion_financiera_muerte_pensionados <- function(proy_pensionados_muertos, suma_asegurada_pensionados) {
  Proyeccion_beneficios_muerte_pensionados <- data.frame(Anno = 2024:(2024 + nrow(proy_pensionados_muertos) - 1), beneficio_muerte = numeric(nrow(proy_pensionados_muertos)))
  
  for (e in 1:nrow(proy_pensionados_muertos)) {
    Proyeccion_beneficios_muerte_pensionados$beneficio_muerte[e] <- (proy_pensionados_muertos$poblacion_hombres[e] + proy_pensionados_muertos$poblacion_mujeres[e]) * suma_asegurada_pensionados * (1.03)^(e - 1)
  }
  
  return(Proyeccion_beneficios_muerte_pensionados)
}




#Proyección financiera de la anualidad
#proy_pensionados_vivos: Es la proyección de pensionados vivos
#pension_mensual: monto de anualidad

Proyeccion_financiera_pension <- function(proy_pensionados_vivos, pension_mensual) {
  anualidad_pensión <- descuento_anual(pension_mensual)
  Proyeccion_financiera_anualidad <- data.frame(Anno = 2024:(2024 + nrow(proy_pensionados_vivos) - 1), anualidad = numeric(nrow(proy_pensionados_vivos)))
  
  for (e in 1:nrow(proy_pensionados_vivos)) {
    Proyeccion_financiera_anualidad$anualidad[e] <- (proy_pensionados_vivos$poblacion_hombres[e] + proy_pensionados_vivos$poblacion_mujeres[e]) * anualidad_pensión * (1.03)^(e - 1)
  }
  
  return(Proyeccion_financiera_anualidad)
}




