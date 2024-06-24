# Proyecciones demográficas

## Caso hombres

proy_hombres_activos <- data.frame(
  Annos = as.numeric(colnames(tablas_activos[[1]])),  # Convertir los nombres de las columnas a numérico
  Valores_Vivas = as.numeric(tablas_activos[[3]]["Total",]),  # Los valores de la fila
  Valores_Muertes = as.numeric(tablas_activos[[1]]["Total",]),
  Muertes_acumuladas = cumsum(as.numeric(tablas_activos[[1]]["Total",]))
)

fig_hombres_vivos_activos <- plot_ly(proy_hombres_activos, x = ~Annos) %>%
  add_lines(y = ~Valores_Vivas, name = 'Vivas', line = list(color = '#B0C4DE')) %>%
  #add_lines(y = ~Valores_Muertes, name = 'Muertas', line = list(color = '#931986')) %>%
  layout(
    title = "Hombres activos vivos según el año",
    xaxis = list(title = 'Años'),
    yaxis = list(title = 'Personas'),
    legend = list(title = list(text = 'Estado'), orientation = 'h', xanchor = 'center', x = 0.5)
  )

fig_hombres_muertes_activos <- plot_ly(proy_hombres_activos, x = ~Annos[2:48]) %>%
  #add_lines(y = ~Valores_Vivas, name = 'Vivas', line = list(color = '#FFA0F5')) %>%
  add_lines(y = ~Valores_Muertes[2:48], name = 'Muertas', line = list(color = '#36648B')) %>%
  layout(
    title = "Muertes de hombres activos según el año",
    xaxis = list(title = 'Años'),
    yaxis = list(title = 'Personas'),
    legend = list(title = list(text = 'Estado'), orientation = 'h', xanchor = 'center', x = 0.5)
  )
  
fig_hombres_acum_muertes_activos <- plot_ly(proy_hombres_activos, x = ~Annos[2:48]) %>%
  #add_lines(y = ~Valores_Vivas, name = 'Vivas', line = list(color = '#FFA0F5')) %>%
  add_lines(y = ~Muertes_acumuladas[2:48], name = 'Muertas', line = list(color = '#36648B')) %>%
  layout(
    title = "Muertes acumuladas de hombres activos según el año",
    xaxis = list(title = 'Años'),
    yaxis = list(title = 'Personas'),
    legend = list(title = list(text = 'Estado'), orientation = 'h', xanchor = 'center', x = 0.5)
  )

## Caso mujeres

proy_mujeres_activos <- data.frame(
  Annos = as.numeric(colnames(tablas_activos[[4]])),  # Convertir los nombres de las columnas a numérico
  Valores_Vivas = as.numeric(tablas_activos[[4]]["Total",]),  # Los valores de la fila
  Valores_Muertes = as.numeric(tablas_activos[[2]]["Total",]),
  Muertes_acumuladas = cumsum(as.numeric(tablas_activos[[2]]["Total",]))
)

fig_mujeres_vivas_activas <- plot_ly(proy_mujeres_activos, x = ~Annos) %>%
  add_lines(y = ~Valores_Vivas, name = 'Vivas', line = list(color = '#FFA0F5')) %>%
  #add_lines(y = ~Valores_Muertes, name = 'Muertas', line = list(color = '#931986')) %>%
  layout(
    title = "Mujeres activas vivas según el año",
    xaxis = list(title = 'Años'),
    yaxis = list(title = 'Personas'),
    legend = list(title = list(text = 'Estado'), orientation = 'h', xanchor = 'center', x = 0.5)
  )

fig_mujeres_muertes_activas <- plot_ly(proy_mujeres_activos, x = ~Annos[2:48]) %>%
  #add_lines(y = ~Valores_Vivas, name = 'Vivas', line = list(color = '#FFA0F5')) %>%
  add_lines(y = ~Valores_Muertes[2:48], name = 'Muertas', line = list(color = '#931986')) %>%
  layout(
    title = "Muertes de mujeres activas según el año",
    xaxis = list(title = 'Años'),
    yaxis = list(title = 'Personas'),
    legend = list(title = list(text = 'Estado'), orientation = 'h', xanchor = 'center', x = 0.5)
  )


fig_mujeres_acum_muertes_activas <- plot_ly(proy_mujeres_activos, x = ~Annos[2:48]) %>%
  #add_lines(y = ~Valores_Vivas, name = 'Vivas', line = list(color = '#FFA0F5')) %>%
  add_lines(y = ~Muertes_acumuladas[2:48], name = 'Muertas', line = list(color = '#931986')) %>%
  layout(
    title = "Muertes acumuladas de mujeres activas según el año",
    xaxis = list(title = 'Años'),
    yaxis = list(title = 'Personas'),
    legend = list(title = list(text = 'Estado'), orientation = 'h', xanchor = 'center', x = 0.5)
  )

rm(proy_hombres_activos)
rm(proy_mujeres_activos)