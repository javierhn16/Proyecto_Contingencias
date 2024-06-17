
# Lectura de las tablas de mortalidad
qx_masculino <- read_excel('data/tavid2000-2150.xlsx', sheet = "Sexo_1_limpio") %>%
  mutate(across(everything(), as.numeric))

qx_femenino <- read_excel('data/tavid2000-2150.xlsx', sheet = "Sexo_2_limpio") %>%
  mutate(across(everything(), as.numeric))

# Lectura de la base de datos con los empleados
base_empleados <- read_excel('data/Base de datos.xlsx')  

# Transformaciones en la base de empleados
# Se agrega columna "Edad"
# Se cambia la columna "Sexo" a factores numéricos 
base_empleados <- mutate(base_empleados,
                         Edad = 2023 - as.numeric(format(`Fecha de nacimiento`, "%Y")),
                         Sexo = as.numeric(recode(Sexo, 'M' = '0', 'F' = '1'))
                         )




