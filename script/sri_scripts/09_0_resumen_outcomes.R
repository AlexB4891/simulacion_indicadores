library(tidyverse)

tabla_trabajo <- read_rds("01_DATA/APS/tabla_modelos_aps_101.rds")

unbalanced <- read_rds("01_DATA/APS/procesados/aps_unbalanced_panel.rds")

balanced <- read_rds("01_DATA/APS/procesados/aps_balanced_panel.rds")

semibalanced <- read_rds("01_DATA/APS/procesados/aps_semibalanced_panel.rds")


sumarizador <- function(tabla,
                        variable){
  
  # variable_text <- variable
  
  # browser()
  
  tabla %>% 
    filter(anio_fiscal <= 2015,
           if_any(c(control,treatment),~.x == 1)) %>% 
    mutate(treatment = factor(treatment,levels = c(0,1),labels = c("control","treatment"))) %>% 
    group_by(treatment) %>% 
    summarise(across(
      .cols = one_of(variable),
      .fns = list(
        mean = ~mean(.x,na.rm = T),
        sd = ~sd(.x,na.rm = T),
        median = ~median(.x,na.rm = T)
      ),
      .names = "{.fn}"
    ),
    firms = n_distinct(identificacion_informante_anon)) %>% 
    ungroup() %>% 
    pivot_longer(cols = c(mean,
                          sd,
                          median,
                          firms),
                 names_to = "statistic",
                 values_to = variable) 
    
}



outcomes <- c("hhi_th"                        , "beneficiarios_total"      ,      "share_pff",
               "share_ext"                    ,  "share_nac"               ,       "porcentage_pff",
               "porcentage_ext"               ,  "porcentage_nac"          ,       "net_profits",
               "taxable_profits"              ,  "local_sales"             ,       "exports",
               "total_sales"                  ,  "labor_costs"             ,       "total_assets",
               "total_capital"                ,  "total_income"            ,       "tax_liability" )


investments_tax <- c("excess_anticipate"            ,  "current_investment"      ,       "long_term_inv_subasoc",
      "long_term_inv_joint"          ,  "long_term_inv_other"     ,       "gross_profit_margin",
      "net_profit_margin"            ,  "profitability"           ,       "return_on_assets",
      "tasa_ir"  )

external_operations <- c(
  "ext_650" ,                     
  "ext_635","pff_650","ext_630" ,                      
  "ext_625","ext_405","ext_645" ,                      
  "ext_415","ext_615","pff_415" ,                      
  "pff_615","pff_630","pff_640" ,                      
  "ext_640","pff_645","pff_720" ,                      
  "pff_625","ext_720","pff_405" ,                      
  "pff_635","ext_710","ext_410" ,                      
  "pff_410","pff_710"                       
)

resumen_total <- list(
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
  imap(~{
    
    tabla_resumen <-  .x
    
    map(outcomes,~sumarizador(tabla = tabla_resumen,variable = .x)) %>% 
      reduce(inner_join) %>% 
      mutate(conjunto = .y)
    
    
  }) %>% 
  reduce(bind_rows)

write_tsv(x = resumen_total,file = "03_RESULTADOS/WB_PB_INVERSIONES/20230224/Tablas/resumenes_outcomes_ejercicio.txt")

resumen_total_2 <- list(
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
  imap(~{
    
    tabla_resumen <-  .x
    
    map(investments_tax,~sumarizador(tabla = tabla_resumen,variable = .x)) %>% 
      reduce(inner_join) %>% 
      mutate(conjunto = .y)
    
    
  }) %>% 
  reduce(bind_rows)

write_tsv(x = resumen_total_2,file = "03_RESULTADOS/WB_PB_INVERSIONES/20230224/Tablas/resumenes_inversiones.txt")

resumen_total_3 <- list(
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
  imap(~{
    
    tabla_resumen <-  .x
    
    map(external_operations,~sumarizador(tabla = tabla_resumen,variable = .x)) %>% 
      reduce(inner_join) %>% 
      mutate(conjunto = .y)
    
    
  }) %>% 
  reduce(bind_rows)

write_tsv(x = resumen_total_3,file = "03_RESULTADOS/WB_PB_INVERSIONES/20230224/Tablas/resumenes_exterior.txt")

