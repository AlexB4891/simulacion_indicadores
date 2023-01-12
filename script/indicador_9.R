
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(scales)
library(flextable)

#setwd("E:/DTO_ESTUDIOS_E2/B_INVESTIGADORES_EXTERNOS/202201_BANCO_MUNDIAL_PBACHAS")

# Reading -----------------------------------------------------------------

#tabla_trabajo <- read_rds("01_DATA/APS/procesados/aps_101_new_vars.rds")
#balanced <- read_rds("01_DATA/APS/procesados/aps_balanced_panel.rds")
#semibalanced <- read_rds("01_DATA/APS/procesados/aps_semi_balanced_panel.rds")






# Lectura de la tabla general ---------------------------------------------

tabla_trabajo <- read_rds("data/base_expandida.rds")

balanced <- read_rds("data/aps_balanced_panel.rds")

semibalanced <- read_rds("data/aps_semi_balanced_panel.rds")

tabla_trabajo <- tabla_trabajo %>% 
  slice_sample(prob = 0.3)





# Indicadores -------------------------------------------------------------


porcentaje_declarado <- list(
  tabla_trabajo %>% 
    filter(dummy_aps_declarado_101 == 1,
           dummy_revisar == 0) %>% 
    select(anio_fiscal,identificacion_informante_anon,porcentaje_pff) %>% 
    mutate(panel = "Desbalanceado"),
  tabla_trabajo  %>% 
    filter(dummy_aps_declarado_101 == 1,
           dummy_revisar == 0) %>% 
    inner_join(balanced) %>% 
    select(anio_fiscal,identificacion_informante_anon,porcentaje_pff) %>% 
    mutate(panel = "Balanceado"),
  tabla_trabajo   %>% 
    filter(dummy_aps_declarado_101 == 1,
           dummy_revisar == 0) %>% 
    inner_join(semibalanced) %>% 
    select(anio_fiscal,identificacion_informante_anon,porcentaje_pff) %>% 
    mutate(panel = "Semibalanceado")
) %>% 
  reduce(bind_rows)



# Resumen estadistico -----------------------------------------------------

tabla_estadisticas <- porcentaje_declarado %>% 
  filter(anio_fiscal <= 2014) %>% 
  mutate(pocent_nac_no_cero = if_else(porcentaje_nac == 0,NA_real_,porcentaje_pff)) %>% 
  group_by(panel,anio_fiscal) %>% 
  summarise(
    `Obs` = n(),
    `Vacios` = sum(is.na(porcentaje_pff)),
    `Cero's` = sum(porcentaje_pff == 0),
    `Mín` = min(porcentaje_pff_no_cero,na.rm = T),
    Median = median(porcentaje_pff_no_cero,na.rm = T),
    Max = max(porcentaje_pff_no_cero,na.rm = T),
    Media = mean(porcentaje_pfff_no_cero,na.rm = T),
    SD = sd(porcentaje_pff_no_cero,na.rm = T)
  ) %>% 
  mutate(across(c(
    `Mín`,
    Median,
    Max,
    Media,
    SD
  ),
  round,2)) %>% 
  mutate(across(c( `Obs`,
                   `Vacios`,
                   `Cero's`),
                number))

estadistica_preliminar <- flextable(tabla_estadisticas) %>% 
  merge_at(i = 1:3,j = 1) %>% 
  merge_at(i = 4:6,j = 1) %>% 
  merge_at(i = 7:9,j = 1) %>% 
  set_header_labels(
    values = c(anio_fiscal = "Año fiscal",
               panel = "Muestra/Panel")) %>% 
  autofit() %>% 
  theme_zebra() %>% 
  align_text_col( align = "center")


tabla_distribucion <- porcentaje_declarado %>% 
  mutate(panel = factor(panel,levels = c("Desbalanceado","Semibalanceado","Balanceado"))) %>% 
  filter(anio_fiscal <= 2014) %>% 
  mutate(porcentaje_pff_no_cero = if_else(porcentaje_pff == 0,NA_real_,porcentaje_pff)) %>% 
  group_by(panel,identificacion_informante_anon) %>% 
  summarise(porcentaje_pff_1214 = mean(porcentaje_pff_no_cero,na.rm = T))

tabla_distribucion <- tabla_distribucion %>% 
  ungroup() %>% 
  group_by(panel) %>% 
  mutate(mean = mean(porcentaje_pff_1214,na.rm = T),
         sd = sd(porcentaje_pff_1214,na.rm = T),
         median = median(porcentaje_pff_1214,na.rm = T),
         upper = mean + sd,
         lower = mean - sd) %>% 
  rowwise() %>% 
  mutate(dummy = between(porcentaje_pff_1214,lower,upper))

grafico_distribucion <- tabla_distribucion %>% 
  filter(!is.nan(porcentaje_pff_1214)) %>% 
  ggplot(mapping = aes(x = porcentaje_pff_1214,color = panel,fill = panel)) +
  geom_histogram(alpha = 0.3) +
  geom_vline(aes(xintercept = mean,color = panel),linetype = 3,size = 0.75) +
  geom_vline(aes(xintercept = median,color = panel),linetype = 1,size = 0.75) +
  geom_rect(aes(xmin = lower,
                xmax = upper,
                ymin = -Inf,
                ymax=Inf),
            alpha = 0.005,
            fill = "grey",
            color = "grey") +
  facet_wrap(panel ~ .,nrow = 3) +
  theme_minimal() +
  labs(y = "Empresas informantes",
       x = "Participación promedio en paraisos fiscales",
       caption = "1. La linea punteada representa la media\n2. La linea solida representa la mediana\n3. El area gris indica una desviación estandard alrededor de la media") +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0,size = 7)) 

resumen_general <- tabla_distribucion %>% 
  select(panel,mean,sd,median) %>% 
  unique() %>% 
  mutate(across(where(is.numeric),round,2)) %>% 
  flextable() %>% 
  set_header_labels(
    values = c(mean = "Media",
               sd = "Desv. Std.",
               median = "Mediana",
               panel = "Muestra/Panel")) %>% 
  autofit() %>% 
  theme_zebra() %>% 
  align_text_col( align = "center")
