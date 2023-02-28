library(tidyverse)
library(gt)
library(readxl)
library(janitor)



tabla_resumen <- read_tsv("data/resumenes_outcomes_ejercicio.txt")

diccionario <- read_excel("data/database_dictionary.xlsx",skip = 8)

diccionario <- diccionario %>% 
  clean_names()

names(tabla_resumen)

make_latex_table <- function(summ_table,
                             variables){
  
  browser()
  
  labels_df <- diccionario %>% 
    filter(name_in_the_database %in% variables) %>%   
    replicate(n = 2,expr = list(.)) %>% 
    map2(c("control","treatment"),
         ~.x %>% 
           mutate(name_in_the_database = str_c(name_in_the_database,.y,sep = "_")) %>% 
           select(name_in_the_database,label)) %>% 
    reduce(bind_rows)
  
  
  labels_list <- set_names(nm = labels_df$name_in_the_database,
                           x = str_c(labels_df$label,
                                     "\n",
                                     "(",1:length(labels_list),")"))
  
  # labels_list_2 <- set_names(nm = labels_df$name_in_the_database,
  #                          x = )
  # 
  
  summ_table %>% 
    select(treatment,
           statistic,
           conjunto,
           one_of(variables)) %>% 
    filter(conjunto != "todas_empresas") %>% 
    mutate(conjunto = factor(conjunto,
                             levels = c("panel_desbalanceado",
                                        "panel_balanceado",
                                        "panel_semibalanceado",
                                        "ingreso_positivo",
                                        "tasa_positiva"),
                             labels = c("Panel 1: All firms (Unbalanced panel)",
                                        "Panel 2: Balanced panel",
                                        "Panel 3: Semibalanced panel",
                                        "Panel 4: Firms with income > 0",
                                        "Panel 5: Firms with income tax rate > 0")
                             ),
           statistic = factor(statistic,
                              levels = c("mean","sd","median","firms"),
                              labels = c("Mean","SD", "P50","N"))) %>% 
    pivot_wider(
      names_from = treatment,
      values_from = one_of(variables)) %>% 
    group_by(conjunto) %>% 
    gt() %>% 
    tab_spanner(
      label = "Control (2012-2015)",
      columns = matches("control$")) %>% 
    tab_spanner(
      label = "Treatment (2012-2015)",
      columns = matches("treatment$")) %>% 
    cols_label(!!!labels_list) %>% 
    cols_label(statistic = "") %>%
    fmt_number(columns = where(is.numeric),
               decimals = 2)
    
  
}


make_latex_table(tabla_resumen,
                 c( "hhi_th","beneficiarios_total",
                    "share_pff","share_ext"  ))
