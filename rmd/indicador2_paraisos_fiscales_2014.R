
# Paquetes ----------------------------------------------------------------

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



# Indicador de Tratamiento continuo de uso de paraísos fiscales -----------


# Estadisticas descriptivas año 2014 --------------------------------------


year_2014 <- tabla_simulada %>% 
  filter(anio_fiscal == 2014)

tabla2 <-year_2014 %>% 
  group_by(anio_fiscal) %>% 
  summarize( mean = mean(mean),
             na.rm = TRUE,
             sd = sd(sd),
             median = median(median))


flextable(tabla2) %>% 
  theme_vanilla() %>% 
  autofit() %>% 
  add_footer_lines("Anexo APS") %>% 
  color(part = "footer", color = "#755440") %>% 
  set_caption(caption = "Estadisticas descriptivas paraísos fiscales año 2014")



# Histograma --------------------------------------------------------------


hist(year_2014$beneficiarios_pff)





