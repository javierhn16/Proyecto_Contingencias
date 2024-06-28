
# Lectura de las tablas de mortalidad de la SUPEN
tablas_supen <- list(
  qx_masculino = read_excel('data/tavid2000-2150.xlsx', sheet = "Sexo_1_limpio") %>%
    mutate(across(everything(), as.numeric)),
  qx_femenino = read_excel('data/tavid2000-2150.xlsx', sheet = "Sexo_2_limpio") %>%
    mutate(across(everything(), as.numeric))
)

# Lectura de la base de datos con los empleados
base_empleados <- read_excel('data/Base de datos.xlsx')

# Transformaciones en la base de empleados
base_empleados <- base_empleados %>% 
  mutate(Edad = 2023 - as.numeric(format(`Fecha de nacimiento`, "%Y")),
         Sexo = if_else(Sexo == 'M', 1, 2)) %>% 
  select(-`Fecha de nacimiento`)

# Limpia los nombres de la base_empleados
colnames(base_empleados) <- c('id', 'sexo', 'edad')

# Extra (es lo mismo de antes)
ABC <- read_excel('data/Base de datos.xlsx') %>%
  mutate(
    `Fecha de nacimiento` = 2023 - as.numeric(format(`Fecha de nacimiento`, "%Y")),
    Sexo = as.numeric(recode(Sexo, 'M' = '1', 'F' = '2'))
  )
colnames(ABC)[2] <- "Edad"

SUPEN <- read_excel("data/tavid2000-2150_original.xls") %>%
  mutate(across(everything(), as.numeric))


