
# Paquetes} ---------------------------------------------------------------

#install.packages("gt")
#install.packages("flextable")
#install.packages("magrittr")




# Librerias ---------------------------------------------------------------

library(tidyverse)
library(readr)
library(gt)
library(scales)
library(rlang)
library(ggplot2)
library(flextable)
library(magrittr)


# Lectura de la tabla general ---------------------------------------------

tabla_simulada <- readRDS("C:/Users/andre/Downloads/tabla_simulada.rds")



# Indicador de beneficiarios paraísos fiscales sobre totales -----------

# Estadisticas descriptivas --------------------------------------------

tabla_simulada3 <- tabla_simulada %>% 
  group_by(anio_fiscal) %>% 
  summarize( mean = mean(porcentaje_pff),
             na.rm = TRUE,
             sd = sd(porcentaje_pff),
             median = median(porcentaje_pff))


# Histograma --------------------------------------------------------------


hist(tabla_simulada1$porcentaje_pff)




