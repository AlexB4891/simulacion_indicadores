
source("scripts/analisis_aps/data_generation/00_funciones.R")

library(readxl)
library(janitor)
library(tidyverse)
library(datawizard)
library(fixest)
library(furrr)

diccionario <- read_excel("database_dictionary.xlsx",skip = 8) %>% 
  clean_names() %>% 
  rowid_to_column()

tabla_trabajo <- read_rds("01_DATA/APS/tabla_modelos_aps_101.rds")

unbalanced <- read_rds("01_DATA/APS/procesados/aps_unbalanced_panel.rds")

balanced <- read_rds("01_DATA/APS/procesados/aps_balanced_panel.rds")

semibalanced <- read_rds("01_DATA/APS/procesados/aps_semibalanced_panel.rds")


generar_todos_insumos <- function(tabla_general,
                                  titulo_panel){
  
  diccionario %>% 
    filter(between(rowid,44,63)) %>% 
    mutate(across(c(log,winsorize),as.logical)) %>% 
    split(.$name_in_the_database) %>% 
    map(~{
      
      logar <- .x %>% pull(log)
      win <- .x %>% pull(winsorize)
      
      vari <- .x %>% pull(name_in_the_database)
      lab <- .x %>% pull(label)
      
      
      example <-  generate_all_summaries(panel_data = tabla_general,
                                         variable = vari,
                                         label = lab,
                                         panel_title = titulo_panel,
                                         log = logar,
                                         winsorise = win)
      
      if(!is.null(example)){
        
      
      titulo_panel <- str_remove_all(titulo_panel,"[:punct:]") %>% 
        str_replace_all(" ","_")
      
      ggsave(filename = str_c("03_RESULTADOS/WB_PB_INVERSIONES/20230303/Graficos/distributions/distribucion_",
                              vari,"_",titulo_panel,".png"),
             plot = example[[1]],width = 7,height = 5.5)
      
      write_tsv(x = example[[2]],file = str_c("03_RESULTADOS/WB_PB_INVERSIONES/20230303/Tablas/summ_tables/summ_",
                                              vari,"_",titulo_panel,".txt"))
      
      write_tsv(x = example[[3]],file = str_c("03_RESULTADOS/WB_PB_INVERSIONES/20230303/Tablas/time_tables/time_",
                                              vari,"_",titulo_panel,".txt"))
      
      ggsave(filename = str_c("03_RESULTADOS/WB_PB_INVERSIONES/20230303/Graficos/time_plots/diff_diff_",
                              vari,"_",titulo_panel,".png"),
             plot = example[[4]],width = 7,height = 5.5)
      
      # write_tsv(x = example[[5]][[1]],file = str_c("03_RESULTADOS/WB_PB_INVERSIONES/20230303/Tablas/model_tables/model_",
      #                                         vari,"_",titulo_panel,".txt"))
      # 
      # write_tsv(x = example[[5]][[2]],file = str_c("03_RESULTADOS/WB_PB_INVERSIONES/20230303/Tablas/performance_tables/performance_",
      #                                              vari,"_",titulo_panel,".txt"))
      # 
      }
      
    })
  
}

??furrr::future_imap

plan(multisession, workers = 6)

list(
  `Panel 1: All firms (Unbalanced panel)` = tabla_trabajo %>% 
    inner_join(unbalanced),
  `Panel 2: Balanced panel` = tabla_trabajo %>% 
    inner_join(balanced),
  `Panel 3: Semi-Balanced panel` = tabla_trabajo %>% 
    inner_join(semibalanced),
  `Panel 4: Positive income` = tabla_trabajo %>% 
    filter(total_income > 0),
  `Panel 5: Positive income tax rate` = tabla_trabajo %>% 
    filter(tasa_ir > 0),
  `Panel 6: Balanced panel with positive I.T.R` = tabla_trabajo %>% 
    inner_join(balanced) %>% 
    filter(tasa_ir > 0)
) %>% 
  future_iwalk(.f = ~{
    
    generar_todos_insumos(tabla_general = .x,titulo_panel = .y)
    
  })
