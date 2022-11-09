library(flextable)
library(tidyverse)


setwd("E:/DTO_ESTUDIOS_E2/B_INVESTIGADORES_EXTERNOS/202201_BANCO_MUNDIAL_PBACHAS")

tabla <- c("aps_declarado","aps_declarado_101","revisar") %>% 
  set_names() %>% 
  str_c("resumenes/aps_analisis/unbalanced/conteo/conteo_dummy_",.,".txt") %>% 
  map(read_tsv)

totales <- tabla %>% 
  map2(.y = c(1,0,1),
       ~.x %>% filter(if_any(.cols = 2,~.x == .y))) %>% 
  map(rowwise) %>% 
  map(mutate,
      across(.cols = 2,
             .fns = ~sum(utilidad_gravable_3560_vacio,
                         dummy_perdida ,
                         dummy_utilidad,
                         dummy_cero,na.rm = T))) %>% 
  map(select,1:2) %>% 
  reduce(inner_join) %>% 
  rename_with(str_remove,pattern = "dummy_") %>% 
  select(anio_fiscal,
         aps_declarado,
         aps_declarado_101,
         revisar)

tabla_lista <- totales %>% 
  mutate(across(c(
    aps_declarado_101,
                  revisar),
                ~ str_c(scales::number(.x),"\n(",round((.x/aps_declarado)*100,2),"%)"))) %>%
  mutate(across(c(anio_fiscal,aps_declarado),~ scales::number(.x)))  


ft_inconsistencias <- tabla_lista %>% 
  flextable() %>% 
  autofit() %>% 
  theme_zebra() %>% 
  align_text_col( align = "center") %>% 
  set_header_labels(
    values = c(anio_fiscal = "Año fiscal",
               aps_declarado = "Total de anexos APS",
               aps_declarado_101 = "Omisión APS",
               revisar = "Inconsistencias")) %>% 
  footnote(i = 1,
           j=3:4,
           value = as_paragraph(c("En parentesis se presenta el porcentaje respecto al total de anexos APS")),
           part = "header") 
  
  # save_as_docx(ft_inconsistencias, 
  #              path = "resumenes/aps_analisis/tabla_incon.docx")
