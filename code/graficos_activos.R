# Proyecciones demográficas

## Caso vivos

proy_activos <- data.frame(
  Annos = as.numeric(colnames(tablas_activos[[1]])),  # Convertir los nombres de las columnas a numérico
  Hombres_Vivos = as.numeric(tablas_activos[[3]]["Total",]),  # Los valores de la fila
  Mujeres_Vivas = as.numeric(tablas_activos[[4]]["Total",]),  # Los valores de la fila
  Hombres_Muertes = as.numeric(tablas_activos[[1]]["Total",]),
  Mujeres_Muertes = as.numeric(tablas_activos[[2]]["Total",]),
  Muertes_homb_acumuladas = cumsum(as.numeric(tablas_activos[[1]]["Total",])),
  Muertes_muj_acumuladas = cumsum(as.numeric(tablas_activos[[2]]["Total",]))
)

fig_activos_vivos <- plot_ly(proy_activos, x = ~Annos) %>%
  add_bars(y = ~Hombres_Vivos, name = 'Hombres', marker = list(color = 'royalblue4')) %>%
  add_bars(y = ~Mujeres_Vivas, name = 'Mujeres', marker = list(color = '#931986')) %>%
  layout(
    title = "Personas activas vivas según el año y sexo",
    xaxis = list(title = list(text = 'Años', standoff = 12)),
    yaxis = list(title = 'Personas'),
    barmode = 'stack',
    legend = list(title = list(text = 'Sexo'), orientation = 'h', xanchor = 'center', x = 0.5),
    margin = list(b = 50, t = 50)
  )

## Caso muertos

fig_activos_muertos <- plot_ly(proy_activos, x = ~Annos) %>%
  add_bars(y = ~Muertes_homb_acumuladas, name = 'Hombres', marker = list(color = 'royalblue4')) %>%
  add_bars(y = ~Muertes_muj_acumuladas, name = 'Mujeres', marker = list(color = '#931986')) %>%
  layout(
    title = "Muertes acumuladas de empleados activos vivos según el año y sexo",
    xaxis = list(title = list(text = 'Años', standoff = 12)),
    yaxis = list(title = 'Personas'),
    barmode = 'stack',
    legend = list(title = list(text = 'Sexo'), orientation = 'h', xanchor = 'center', x = 0.5),
    margin = list(b = 50, t = 50)
  )
