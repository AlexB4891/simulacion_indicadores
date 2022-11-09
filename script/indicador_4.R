
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(scales)
library(flextable)

setwd("E:/DTO_ESTUDIOS_E2/B_INVESTIGADORES_EXTERNOS/202201_BANCO_MUNDIAL_PBACHAS")

# Reading -----------------------------------------------------------------

tabla_trabajo <- read_rds("01_DATA/APS/procesados/aps_101_new_vars.rds")
balanced <- read_rds("01_DATA/APS/procesados/aps_balanced_panel.rds")
semibalanced <- read_rds("01_DATA/APS/procesados/aps_semi_balanced_panel.rds")

 
# tabla_trabajo <- tabla_trabajo %>% 
#   mutate(hhi = beneficiarios_pff/total_benef_final)

tabla_trabajo <- tabla_trabajo %>%
  mutate(participacion_cuadrado = porcentaje_efectivo^2) %>% 
  group_by(anio_fiscal, identificacion_informante_anon) %>% 
  summarise(hhi = sum(participacion_cuadrado, na.rm = TRUE))


# Indicadores -------------------------------------------------------------


porcentaje_declarado <- list(
  tabla_trabajo %>% 
    filter(dummy_aps_declarado_101 == 1,
           dummy_revisar == 0) %>% 
    select(anio_fiscal,identificacion_informante_anon,hhi) %>% 
    mutate(panel = "Desbalanceado"),
  tabla_trabajo  %>% 
    filter(dummy_aps_declarado_101 == 1,
           dummy_revisar == 0) %>% 
    inner_join(balanced) %>% 
    select(anio_fiscal,identificacion_informante_anon,hhi) %>% 
    mutate(panel = "Balanceado"),
  tabla_trabajo   %>% 
    filter(dummy_aps_declarado_101 == 1,
           dummy_revisar == 0) %>% 
    inner_join(semibalanced) %>% 
    select(anio_fiscal,identificacion_informante_anon,hhi) %>% 
    mutate(panel = "Semibalanceado")
) %>% 
  reduce(bind_rows)



# Resumen estadistico -----------------------------------------------------

tabla_estadisticas <- porcentaje_declarado %>% 
  filter(anio_fiscal <= 2014) %>% 
  mutate(hhi = if_else(hhi == 0,NA_real_,hhi)) %>% 
  group_by(panel,anio_fiscal) %>% 
  summarise(
    `Obs` = n(),
    `Vacios` = sum(is.na(hhi)),
    `Cero's` = sum(hhi == 0),
    `Mín` = min(hhi,na.rm = T),
    Median = median(hhi,na.rm = T),
    Max = max(hhi,na.rm = T),
    Media = mean(hhi,na.rm = T),
    SD = sd(hhi,na.rm = T)
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
  mutate(hhi = if_else(hhi == 0,NA_real_,hhi)) %>% 
  group_by(panel,identificacion_informante_anon) %>% 
  summarise(hhi_14 = mean(hhi,na.rm = T))

tabla_distribucion <- tabla_distribucion %>% 
  ungroup() %>% 
  group_by(panel) %>% 
  mutate(mean = mean(hhi_14,na.rm = T),
         sd = sd(hhi_14,na.rm = T),
         median = median(conc_14,na.rm = T),
         upper = mean + sd,
         lower = mean - sd) %>% 
  rowwise() %>% 
  mutate(dummy = between(hhi_14,lower,upper))

grafico_distribucion <- tabla_distribucion %>% 
  filter(!is.nan(hhi_14)) %>% 
  ggplot(mapping = aes(x = hhi_14,color = panel,fill = panel)) +
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
       x = "Concentración de participes en paraisos fiscales",
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
  