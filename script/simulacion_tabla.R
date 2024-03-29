
# Librerias ---------------------------------------------------------------

library(tidyverse)
library(gt)
library(scales)
library(rlang)


# Lectura de la tabla general ---------------------------------------------

conteos <- read_tsv(file = "data/participacion_dummy_aps_declarado.txt")



# Me quedo con las variables de interes para la simulación ----------------

conteos <- conteos %>%
 mutate(utilidad_positiva = obs - dummy_perdida - dummy_cero) %>%
 select(-obs,-no_na) %>%
 filter(dummy_aps_declarado == 1)


# Paso a un formato "long" ------------------------------------------------


conteos_long <- conteos %>%
 pivot_longer(cols = c("dummy_perdida","dummy_cero","utilidad_positiva"),
              names_to = "resultado_fiscal",
              values_to = "frecuencia"
              )


# Expandir la base de datos ------------------------------------------------

base_expandida <- uncount(data = conteos_long,
                         weights = frecuencia)



# asignación de la participación ------------------------------------------
# Supuesto: la participación declarada sigue una distribución normal

# Semilla aleatoria, para reproducibilidad:

set.seed(2348761)
# rnorm(100,mean = 10,sd = 2)

base_expandida <- base_expandida %>%
  rowwise() %>%
  mutate(percent_declarado = rnorm(n = n(),mean = mean,sd = sd),
         percent_declarado = if_else(percent_declarado > 100,
                                     true = 100,
                                     false = percent_declarado))


# Asignar un ID de las empresas ------------------------------------------------

base_expandida <- base_expandida %>%
 group_by(anio_fiscal) %>%
 mutate(identificacion_informante_anon = row_number())

# Siempre que suses un group_by hay que desagrupar!

# Utilidades --------------------------------------------------------------
# Supuesto: las utilidades tienen una distribución normal,
# y la media y desviación standar es constante en el tiempo

set.seed(2348761)

base_expandida <- base_expandida %>%
 ungroup() %>%
 mutate(utilidades = case_when(
   resultado_fiscal == "dummy_perdida" ~ - abs(rnorm(n = n(),mean = 150000,sd = 100000)),
   resultado_fiscal == "dummy_cero" ~ 0,
   resultado_fiscal == "utilidad_positiva" ~ abs(rnorm(n = n(),mean = 150000,sd = 100000))
 ))


# Añadiendo numeros de participaes y participaci[on en paraisos fiscales --------


base_expandida <- base_expandida %>%
 ungroup() %>%
 mutate(
   beneficiarios_finales = rpois(n = n(),lambda = 70),
   beneficiarios_pff = rpois(n = n(),lambda = 10),
   beneficiarios_ext = rpois(n = n(),lambda = 20),
   beneficiarios_nac = beneficiarios_finales - beneficiarios_pff - beneficiarios_ext
 )


# Porcentajes participación -----------------------------------------------


base_expandida <- base_expandida %>%
 ungroup() %>%
 mutate(
    porcentaje_ext = rnorm(n = n(),mean = 10,10),
    porcentaje_nac = rnorm(n = n(),mean = 20,10),
    porcentaje_no_pff = rnorm(n = n(),mean = 1.5,10),
    porcentaje_pff = percent_declarado - porcentaje_ext - porcentaje_nac - porcentaje_no_pff,
    porcentaje_pff = if_else(porcentaje_pff < 0,0,porcentaje_pff)
 )




#todas las empresas tienen un Formulario APS 

base_expandida <- base_expandida %>% 
  mutate(dummy_aps_declarado_101 = 1,
         dummy_revisar = 0)




write_rds(x = base_expandida,file = "data/base_expandida.rds")

# El insumo de los siguientes pasos es la "base expandida", a partir de esta creamos los paneles
# primera generas con las lineas anteriores


# Indicadores -------------------------------------------------------------

# Número de beneficiarios finales dentro de residencia en paraísos fiscales

balanced <- base_expandida %>%
  group_by(identificacion_informante_anon) %>% 
  summarise(n = n_distinct(anio_fiscal)) %>%
  filter(n == 8) %>% 
  select(identificacion_informante_anon)

write_rds(balanced,"data/aps_balanced_panel.rds")

semibalanced <- base_expandida %>% 
  mutate(pre = if_else( anio_fiscal <= 2015, 
                        true = 1, 
                        false = 0),
         post = if_else( anio_fiscal > 2015, 
                        true = 1, 
                        false = 0)) 

semibalanced <- semibalanced %>% 
  group_by(identificacion_informante_anon) %>% 
  summarise(pre = any(pre == 1),
            post = any(post == 1)) %>% 
  filter(pre == 1 & post == 1) %>% 
  select(identificacion_informante_anon)

write_rds(semibalanced,"data/aps_semi_balanced_panel.rds")


# Variables del formulario del impuesto a la renta ------------------------

# Sobre esta base (base_expandida) le vamos a simular  a las variables ventas y utilidades, al final calculamos el net profit ratio (profitability)

# Ventas netas: vln_eaf_tdc_1800 (nombre de la variable) 
# Utilidad del ejercicio: utilidad_ejercicio_3420 

# Simulacmos las variables como normales con media (10000) y una desviacion grande
# Los valores seran distintos para cada varaibles

# Sobre esta base le vamos a simular  a las variables ventas y utilidades, al final calculamos el net profit ratio (profitability)

#Nuevas variables de ventas y utilidades para la obtencion del ratio de utilidad neta de la empresa

base_expandida <- base_expandida %>% 
  rowwise() %>%
  mutate(vln_eaf_tdc_1800 = rnorm(n = n(),mean = 100000,5300),
         utilidad_ejercicio_3420 = rnorm(n = n(),mean = 100000,4150),
         net_profit_ratio = (utilidad_ejercicio_3420/vln_eaf_tdc_1800)*100)


base_expandida <- read_rds("data/base_expandida.rds")

