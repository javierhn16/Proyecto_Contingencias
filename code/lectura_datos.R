
# Lectura de las tablas de mortalidad
qx_masculino <- read_excel('data/tavid2000-2150.xlsx', sheet = "Sexo_1_limpio")
qx_femenino <- read_excel('data/tavid2000-2150.xlsx', sheet = "Sexo_2_limpio")

# Lectura de la base de datos con los empleados
base_empleados <- read_excel('data/Base de datos.xlsx')
