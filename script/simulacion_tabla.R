
# Librerias ---------------------------------------------------------------

library(tidyverse)
library(gt)
library(scales)
library(rlang)


# Lectura de la tabla general ---------------------------------------------

#conteos <- read_tsv(file = "data/participacion_dummy_aps_declarado.txt")



# Me quedo con las variables de interes para la simulación ----------------

#conteos <- conteos %>% 
#  mutate(utilidad_positiva = obs - dummy_perdida - dummy_cero) %>% 
#  select(-obs,-no_na) %>% 
#  filter(dummy_aps_declarado == 1)


# Paso a un formato "long" ------------------------------------------------


#conteos_long <- conteos %>% 
#  pivot_longer(cols = c("dummy_perdida","dummy_cero","utilidad_positiva"),
#               names_to = "resultado_fiscal",
#               values_to = "frecuencia"
#               )


# Expandir la base de datos ------------------------------------------------

#base_expandida <- uncount(data = conteos_long,
#                          weights = frecuencia)



# asignación de la participación ------------------------------------------
# Supuesto: la participación declarada sigue una distribución normal

# Semilla aleatoria, para reproducibilidad:

#set.seed(2348761)
# rnorm(100,mean = 10,sd = 2)

#base_expandida <- base_expandida %>% 
  # slice(1:19) %>% 
  # rowwise() %>% 
#  mutate(percent_declarado = rnorm(n = n(),mean = mean,sd = sd),
#         percent_declarado = if_else(percent_declarado > 100,
#                                     true = 100,
#                                     false = percent_declarado))


# Asignar un ID de las empresas ------------------------------------------------

#base_expandida <- base_expandida %>% 
#  group_by(anio_fiscal) %>% 
#  mutate(identificacion_informante_anon = row_number())

# Siempre que suses un group_by hay que desagrupar!

# Utilidades --------------------------------------------------------------
# Supuesto: las utilidades tienen una distribución normal,
# y la media y desviación standar es constante en el tiempo

#set.seed(2348761)

#base_expandida <- base_expandida %>% 
#  ungroup() %>% 
#  mutate(utilidades = case_when(
#    resultado_fiscal == "dummy_perdida" ~ - abs(rnorm(n = n(),mean = 150000,sd = 100000)),
#    resultado_fiscal == "dummy_cero" ~ 0,
#    resultado_fiscal == "utilidad_positiva" ~ abs(rnorm(n = n(),mean = 150000,sd = 100000))
#  ))


# Añadiendo numeros de participaes y participaci[on en paraisos fiscales --------


#base_expandida <- base_expandida %>% 
#  ungroup() %>% 
#  mutate(
#    beneficiarios_finales = rpois(n = n(),lambda = 70),
#    beneficiarios_pff = rpois(n = n(),lambda = 10),
#    beneficiarios_ext = rpois(n = n(),lambda = 20),
#    beneficiarios_nac = beneficiarios_finales - beneficiarios_pff - beneficiarios_ext
#  )


# Porcentajes participación -----------------------------------------------


#base_expandida <- base_expandida %>% 
#  ungroup() %>% 
#  mutate(
#     porcentaje_ext = rnorm(n = n(),mean = 10,10),
#     porcentaje_nac = rnorm(n = n(),mean = 20,10),
#     porcentaje_no_pff = rnorm(n = n(),mean = 1.5,10),
#     porcentaje_pff = percent_declarado - porcentaje_ext - porcentaje_nac - porcentaje_no_pff,
#     porcentaje_pff = if_else(porcentaje_pff < 0,0,porcentaje_pff)
#  )

#write_rds(x = base_expandida,file = "../simulacion_indicadores/tabla_simulada.rds")




# El insumo de los siguientes pasos es la "base expandida", a partir de esta creamos los paneles
# primera generas con las lineas anteriores


# Indicadores -------------------------------------------------------------
balanced <- balanced %>%
  group_by(informante) %>% 
  n_distinct(años) %>% 
  filter(if_else(n == 8, 
                 true = 1,
                 false = 0)) %>% 
  select(id_empresa)


semibalanced <- semibalanced %>% 
  mutate(pre = if_else( year <= 2015, 
                        true = 1, 
                        false = 0),
         pro = if_else( year >= 2015, 
                        true = 1, 
                        false = 0)) %>% 
  filter(pre == 1 &  pro == 1)

# Aqui abajo deben ir con comentarios los guardados de las bases en la carpeta data



