
# Paquetes} ---------------------------------------------------------------

#install.packages("gt")



# Librerias ---------------------------------------------------------------

library(tidyverse)
library(readr)
library(gt)
library(scales)
library(rlang)
library(ggplot2)


# Lectura de la tabla general ---------------------------------------------

tabla_simulada <- readRDS("C:/Users/andre/Downloads/tabla_simulada.rds")



# Indicador de Tratamiento continuo de uso de paraísos fiscales -----------


# participacion de la empresa (sumatoria de los i) y promedio para los 3 años 
# contempla los años 2012,2013 y 2014

#Periodo 2012-2014

year_2012_2014 <- tabla_simulada %>% 
  filter(anio_fiscal <= 2014) %>% 
  mutate(prom_bpff = mean(beneficiarios_pff,na.rm = T)) 



# TABLA POR AÑO MENCIONA EL NA , LA MEDIA , LA DESV STANDAR, 

tabla1 <-year_2012_2014 %>% 
  group_by(anio_fiscal) %>% 
  summarize( mean = mean(mean),
             na.rm = TRUE,
             sd = sd(sd),
             median = median(median))


# Histograma --------------------------------------------------------------


hist(tabla1$mean)




