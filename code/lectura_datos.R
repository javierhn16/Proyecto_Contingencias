
# Lectura de las tablas de mortalidad
qx_masculino <- read_excel('data/tavid2000-2150.xlsx', sheet = "Sexo_1_limpio") %>%
  mutate(across(everything(), as.numeric))

qx_femenino <- read_excel('data/tavid2000-2150.xlsx', sheet = "Sexo_2_limpio") %>%
  mutate(across(everything(), as.numeric))

# Lectura de la base de datos con los empleados
base_empleados <- read_excel('data/Base de datos.xlsx')  

# Transformaciones en la base de empleados
base_empleados <- base_empleados %>% 
  mutate(Edad = 2023 - as.numeric(format(`Fecha de nacimiento`, "%Y")),
         Sexo = if_else(Sexo == 'M', 0, 1))

# Limpia los nombres de la base_empleados
colnames(base_empleados) <- c('id', 'fecha_nacimiento', 'sexo', 'edad')





