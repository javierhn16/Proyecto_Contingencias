# Esta función recibe la lista de la proyección demográfica de empleados activos.

proyeccion_financiera <- function(lista){
  graf_proy_hombres <- data.frame(
    Annos = 2024:2069,  # Convertir los nombres de las columnas a numérico
    Proyeccion =  t(lista[[3]]["Total", 2:47] * 5000000)
  )
  
  graf_proy_mujeres <- data.frame(
    Annos = 2024:2069,  # Convertir los nombres de las columnas a numérico
    Proyeccion = t(lista[[4]]["Total", 2:47] * 5000000)# Los valores de la fila
  )
  
  resultado <- list(graf_proy_hombres, graf_proy_mujeres)
  return(resultado)
}
