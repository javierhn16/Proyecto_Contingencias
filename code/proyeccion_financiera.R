# Esta función recibe la lista de la proyección demográfica de empleados activos.

proyeccion_financiera <- function(lista, inflacion){
  
  # Calcula los factores de inflación
  factores_inflacion <- (1 + inflacion)^(1:46)

  graf_proy <- data.frame(
    Annos = 2024:2069,  # Convertir los nombres de las columnas a numérico
    Proyeccion_Total = t((lista[[4]]["Total", 2:47] + lista[[3]]["Total", 2:47])  * 5000000) * factores_inflacion,
    Proyeccion_H =  t(lista[[3]]["Total", 2:47] * 5000000) * factores_inflacion,
    Proyeccion_M = t(lista[[4]]["Total", 2:47] * 5000000) * factores_inflacion
  )
  resultado <- graf_proy
  return(resultado)
}
