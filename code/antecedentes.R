# Variacion Internual

var_interanual <- read_excel("docs/Antecedentes/variacion_interanual.xlsx")
var_interanual$anyos <- as.Date(paste0(var_interanual$anyos, " - 01 - 01"), format = "%Y - %m - %d")

# Tasa Básica Pasiva

data_tbp <- read_excel("docs/Antecedentes/tasa_basica_pasiva.xlsx", 
                       range = "AJ5:AR371")
data_tbp <- data_tbp[-60, ]

Dia <- 1:365

data_tbp <- cbind(Dia, data_tbp)

# Transformar el dataframe a long
data_tbp_long <- data_tbp %>%
  pivot_longer(cols = -Dia, names_to = "Year", values_to = "Rate") %>%
  mutate(Date = as.Date(paste0(Year, "-", Dia), format = "%Y-%j")) %>%
  arrange(Date)

# Empleados

empleados <- base_empleados %>%
  group_by(edad, sexo) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(Sexo_Nombre = ifelse(sexo == 1, "Hombres", "Mujeres"))

# Poblacion

poblacion_sin_limpiar <- read_excel("docs/Antecedentes/poblacion.xlsx")

poblacion_limpia <- data.frame(
  Annos = 2015:2023,
  Poblacion = t(poblacion_sin_limpiar[6,17:25]),
  Tasa_Mortalidad = t(poblacion_sin_limpiar[17,17:25]),
  Tasa_Mortalidad_H = t(poblacion_sin_limpiar[18,17:25]),
  Tasa_Mortalidad_M = t(poblacion_sin_limpiar[19,17:25]),
  Esperanza = t(poblacion_sin_limpiar[34,17:25]),
  Esperanza_H = t(poblacion_sin_limpiar[35,17:25]),
  Esperanza_M = t(poblacion_sin_limpiar[36,17:25])
)
poblacion_limpia$Annos <- as.Date(paste0(poblacion_limpia$Annos, " - 01 - 01"), format = "%Y - %m - %d")