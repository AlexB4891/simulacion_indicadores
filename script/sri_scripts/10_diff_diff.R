# Diff and diff graphs

library(tidyverse)
library(datawizard)
library(broom)

tabla_trabajo <- read_rds("01_DATA/APS/tabla_modelos_aps_101.rds")


unbalanced <- read_rds("01_DATA/APS/procesados/aps_unbalanced_panel.rds")

balanced <- read_rds("01_DATA/APS/procesados/aps_balanced_panel.rds")

semibalanced <- read_rds("01_DATA/APS/procesados/aps_semibalanced_panel.rds")


df_diff_diff <- function(tabla, 
                           variable,
                           log = FALSE,
                           winsorise = FALSE){
  
  tabla <- tabla %>% 
    filter(if_any(c(control,treatment),~.x == 1)) %>% 
    mutate(treatment = factor(treatment,levels = c(0,1),labels = c("control","treatment")))
  
  if(log){
    
    tabla <- tabla %>% 
      mutate(across(one_of(variable),~log(.x)))
    
  }
  
  
  if(winsorise){
    
    tabla <- tabla %>% 
      mutate(across(one_of(variable),~winsorize(.x,threshold = 0.05)))
    
  }
  
  
  
  
  mean_df <- tabla %>% 
    filter(anio_fiscal <= 2015) %>% 
    group_by(treatment)  %>% 
    summarise(across(.cols = one_of(variable),
                     ~mean(.x,na.rm = T))) %>% 
    rename_with(.cols = one_of(variable),
                ~"mean_1215")
  
  yearly_df <- tabla %>% 
    group_by(anio_fiscal,
             treatment) %>% 
    summarise(across(.cols = one_of(variable),
                     ~mean(.x,na.rm = T))) %>% 
    rename_with(.cols = one_of(variable),
                ~"mean_year")
  
  result_df <- inner_join(mean_df,
                              yearly_df) %>% 
    mutate(ratio = mean_year/mean_1215)
  
  
  mean_df_ind <- tabla %>% 
    filter(anio_fiscal <= 2015) %>% 
    group_by(treatment,
             identificacion_informante_anon)  %>% 
    summarise(across(.cols = one_of(variable),
                     ~mean(.x,na.rm = T))) %>% 
    rename_with(.cols = one_of(variable),
                ~"mean_1215")
  
  yearly_df_ind <- tabla %>% 
    group_by(anio_fiscal,
             identificacion_informante_anon,
             treatment) %>% 
    summarise(across(.cols = one_of(variable),
                     ~mean(.x,na.rm = T))) %>% 
    rename_with(.cols = one_of(variable),
                ~"mean_year")
  
  
  result_df_ind <- inner_join(mean_df_ind,
             yearly_df_ind) %>% 
    mutate(ratio = mean_year/mean_1215,
           pre_post = if_else(anio_fiscal <= 2015,"pre","post"),
           pre_post = factor(pre_post,levels = c("pre","post")))
  

  
  modelo <- lm(data = result_df_ind %>% filter(!is.nan(ratio),
                                               !is.na(ratio),
                                               !is.infinite(ratio)),
               formula = ratio ~ treatment + pre_post +  treatment*pre_post)
  
  modelo_df <- tidy(modelo, conf.int = TRUE)
  
  performance <- glance(modelo)


  return(list(table = result_df,
              model_estimation = modelo_df,
              performance = performance))
  
}

resumen_diff_diff <- list(
  todas_empresas = tabla_trabajo,
  panel_desbalanceado = tabla_trabajo %>% 
    inner_join(unbalanced),
  panel_balanceado = tabla_trabajo %>% 
    inner_join(balanced),
  panel_semibalanceado = tabla_trabajo %>% 
    inner_join(semibalanced),
  ingreso_positivo = tabla_trabajo %>% 
    filter(total_income > 0),
  tasa_positiva = tabla_trabajo %>% 
    filter(tasa_ir > 0)
) %>% 
  map(~df_diff_diff(.x,
                    variable = "tasa_ir"))


resumen_diff_diff <- resumen_diff_diff %>% 
  transpose() %>% 
  map(~imap(.x,~.x %>% mutate(panel = .y)) %>% 
        reduce(bind_rows))


c("tabla_graficos.txt",
  "tabla_modelos.txt",
  "tabla_performance.txt") %>% 
  map2(.y = resumen_diff_diff,
       ~{
         
         file_out <- str_c("03_RESULTADOS/WB_PB_INVERSIONES/20230228/",.x)
         
         write_tsv(.y,file = file_out)
         
       })

plot_df <- df_diff_diff(tabla_trabajo %>%
                          inner_join(balanced),
                        variable = "tasa_ir")


"03_RESULTADOS/WB_PB_INVERSIONES/20230228"

diff_table <- plot_df[[1]] %>% 
  mutate(pre_post = if_else(anio_fiscal <= 2015,"pre","post"),
         pre_post = factor(pre_post,levels = c("pre","post"))) %>% 
  group_by(pre_post,treatment) %>% 
  summarise(ratio = mean(ratio)) %>% 
  pivot_wider(names_from = treatment,values_from = ratio) %>% 
  mutate(difference = treatment - control)

plot_df %>% 
  ggplot() +
  geom_line(aes(x = anio_fiscal,
                y = ratio,
                color = treatment)) +
  geom_point(aes(x = anio_fiscal,
                 y = ratio,
                 color = treatment,
                 shape = treatment)) +
  scale_shape_manual(values = c(4,16)) +
  theme_minimal() 
