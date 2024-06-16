
# Descuenta la anualidad un año para tener la anualidad anual

descuento_anual <- function(cantidad){
  
  # bajo el supuesto de que 0.04 es la tasa convertible mensualmente
  i <- 0.04/12
  v <- 1/(1 + i)
  m <- 0:13
  suma <- cantidad * sum(v^m)
  return(suma)
}

# Función de crecimiento geométrico

# x: Edad en que se asegura
# s: sexo, 1 masculino, 2 femenino
# lista: tabla de mortalidad 

Ia_Geometrica <- function(x, s, lista){
  i <- 0.04
  v <- 1/(1 + i)
  # m: Tiempo máximo de pago
  m <- 65 - x
  suma <- 0
  for (t in 1:m){
    suma <- suma + (v^(t)) * (1.03)^(t-1) * as.double(1 - lista[[s]][x + t - 1, 25 + t - 1])
  }
  return(suma)
}

# Función para los beneficios 

# x: Edad en que se asegura
# s: sexo, 1 masculino, 2 femenino
# lista: tabla de mortalidad 

valor_presente_beneficios <- function(x, s, lista) {
  # tasa anual
  i <- 0.04
  # Valor Presente en caso de fallecimiento 
  VPF <- 0  
  # Valor Presente en caso de sobrevivencia (pago pensión)
  VPS <- 0
  # Auxiliar para obtener probabilidad de sobrevivencia acumulada
  p_x <- 1
  # Auxiliar para conteo del tiempo después de 65 años
  t_p <- 0
  # Beneficio en caso de ser empleado activo primer año
  suma_asegurada_activo <- 5000000
  # Beneficio en caso de ser pensionado primer año
  suma_asegurada_pensionado <- 1000000
  # Pensión anualizada
  anualidad_pensión <- descuento_anual(300000)
  
  for (t in 1:(nrow(lista[[s]]) - x)) {
    #Probabilidad de muerte
    q_t <- as.double(lista[[s]][x + t - 1, 25 + t - 1])
    v_t <- (1 / (1 + i))^t
    #Probabilidad de sobrevivencia acumulada
    p_x <- p_x * (1 - q_t)
    
    #Si la persona es menor de 65 años
    if (x + t - 1 < 65) {
      
      VPF <- VPF + suma_asegurada_activo * (1.03)^(t - 1) * v_t * p_x * q_t
      
      
    } #Al sobrepasar los 65 años, tiene seguro vitalicio y pensión vitalicia
    else if (x + t - 1 >= 65) {
      
      VPF <- VPF + suma_asegurada_pensionado * (1.03)^t_p * v_t * p_x * q_t 
      VPS <- VPS + anualidad_pensión * (1.03)^t_p * v_t * p_x
      t_p <- t_p + 1
      
    }
  }
  
  VP <- VPS + VPF
  return(VP)
}

#Base : Base de empleados 
#Tabla_muerte : Lista de las tabla de mortalidad separada por sexo: 1 (hombre), 2 (mujer)

Calcula_prima_individuales <- function (Base, Tabla_mortal){
  # Crecimiento de la prima para cada empleado 
  
  tabla_resultados <- data.frame(anualidad_creciente = numeric(nrow(Base)))
  
  for (i in 1:nrow(Base)){
    if (Base$Sexo[i] == "M"){
      tabla_resultados$anualidad_creciente[i] <- Ia_Geometrica(Base$Edad[i], 1, Tabla_mortal)
    } else if (Base$Sexo[i] == "F"){
      tabla_resultados$anualidad_creciente[i] <- Ia_Geometrica(Base$Edad[i], 2, Tabla_mortal)
    }
  }
  
  # Función de resultados beneficios
  
  tabla_resultados2 <- data.frame(beneficios = numeric(nrow(Base)))
  
  for (i in 1:nrow(Base)){
    if (Base$Sexo[i] == "M"){
      tabla_resultados2$beneficios[i] <- valor_presente_beneficios(Base$Edad[i], 1, Tabla_mortal)
    } else if (Base$Sexo[i] == "F"){
      tabla_resultados2$beneficios[i] <- valor_presente_beneficios(Base$Edad[i], 2, Tabla_mortal)
    }
  }
  
  # Resultados de primas 
  
  Primas_individuales <- data.frame(Empleado = Base$Id, Primas = tabla_resultados2$beneficios / tabla_resultados$anualidad_creciente)
  
  return(Primas_individuales)
}


Primas<-Calcula_prima_individuales(Base_empleados,Tablas_mortalidad)

print(Primas)
