
# Treatment exposur varibales ---------------------------------------------

# 1. Important national shareholders in intensive tax haven using of tax haven

# Libraries:

library(tidyverse)
library(lubridate)
library(readxl)

# Datos y panel

tabla_trabajo <- read_rds("01_DATA/APS/procesados/aps_mas_f101_df.rds")

unbalanced <- read_rds("01_DATA/APS/procesados/aps_unbalanced_panel.rds")

balanced <- read_rds("01_DATA/APS/procesados/aps_balanced_panel.rds")

semibalanced <- read_rds("01_DATA/APS/procesados/aps_semibalanced_panel.rds")

participacion <- read_rds("01_DATA/APS/aps_unicos_lista.rds")


participacion <- participacion %>% 
  rowid_to_column() 

maximo <- participacion %>% 
  pull(rowid) %>% 
  max()


inicio <- seq(from = 1,to = maximo,by = 1000)

fin <- c(inicio[2:length(inicio)] - 1 ,maximo)

part_list_df <- map2(.x = inicio,
     .y = fin,
     .f = function(i,f){
       
       tabla <- participacion %>% 
         filter(between(rowid,i,f)) %>% 
         mutate(data = map(data,~.x %>% mutate(codigo_pais_accionista = as.character(codigo_pais_accionista)))) %>% 
         unnest() 
       
       msg <- str_c("lineas entre",i,"y",f,"rows:",nrow(tabla),sep = " ")
         
       print(msg)
       
       return(tabla)
       
     })

write_rds(x = part_list_df,file = "01_DATA/APS/lista_unicos_descollapsed.rds",compress = "gz")


hhi <- part_list_df %>% 
  imap(~{
    
    tabla <- .x %>% 
      # filter(!is.na(porcentaje_efectivo)) %>% 
      mutate(part_cuad = porcentaje_efectivo^2,
             part_cuad_th = if_else(paraiso_fiscal_accionista == "S",
                                    part_cuad,
                                    0,
                                    missing = 0)) %>% 
      group_by(anio_fiscal,identificacion_informante_anon) %>%
      summarise(hhi_th = sum(part_cuad_th,na.rm = T))
    
    print(str_c("tabla",.y,"filas",nrow(tabla),sep = " "))
    
    return(tabla)
    
    }) %>% 
  reduce(bind_rows)

write_rds(x = hhi,file = "01_DATA/APS/aps_hhi_tax_haven.rds",compress = "gz")


prominent_individuals <- part_list_df %>% 
  imap(~{
    
    tabla_1 <- .x %>% 
      select(anio_fiscal,
             identificacion_informante_anon,
             porcentaje_efectivo,
             pais_accionista_unico,
             nivel_aps)
    
   tabla_2 <- tabla_1 %>% 
      pull(porcentaje_efectivo) %>% 
      map_dfc(c(nac_10 = 10,nac_20 = 20,nac_50 = 50),
              magrittr::is_weakly_greater_than,
              e1 = .) %>% 
     bind_cols(tabla_1)
    
    
    print(str_c("tabla",.y,"filas",nrow(tabla_2),sep = " "))
    
    return(tabla_2)
    
  }) %>% 
  reduce(bind_rows)

write_rds(x = prominent_individuals,file = "01_DATA/APS/prominent_individuals.rds",compress = "gz")


# Profitability -----------------------------------------------------------

data_101 <- list.files("01_DATA/F101/completo/",
                       pattern = "[0-9]+.rds",full.names = T)[6:13] %>%
  set_names() %>% 
  map(~.x %>% 
        read_rds %>% 
        # names
        select(
          one_of(c(
            # Identifiers:
            
            "NUMERO_IDENTIFICACION_an",
            "id_an",
            "ANIO_FISCAL",
            
            # Assets/Capital
            
            "TOTAL_ACTIVO_1080",
            "TOTAL_ACTIVOS_FIJOS_690",
            "TOTAL_ACTIVOS_DIFERIDOS_780", # Intangibles
            "TOTAL_ACTIVOS_LARGO_PLAZO_1070",
            "TOT_ACTIVO_NO_CORRIENTE_1077",
            
            # Capital/Patrimonio
            
            "TOTAL_PATRIMONIO_NETO_1780",
            
            # Inversiones:
            
            # Corrientes:
            
            "INVERSIONES_CORRIENTES_180",
            
            # No corrientes:
            
            "INC_SUB_ASO_785", 
            "INC_NEG_CON_795",
            "OTR_INVERSIONES_LPL_800",
            
            # Total costos y gastos
            
            "TOTAS_COSTOS_GASTOS_3380"
            
            
          ))) 
  ) %>% 
  reduce(bind_rows) 

data_101 <- data_101 %>% 
  mutate(NUMERO_IDENTIFICACION_an = if_else(is.na(NUMERO_IDENTIFICACION_an),
                                            id_an,NUMERO_IDENTIFICACION_an)) %>% 
  filter(!is.na(NUMERO_IDENTIFICACION_an))

data_101 <- data_101 %>% 
  rename_with(str_to_lower) %>% 
  rename(identificacion_informante_anon = numero_identificacion_an)

hhi <- read_rds("01_DATA/APS/aps_hhi_tax_haven.rds")

# Variables de la base final ----------------------------------------------

tabla_modelos <- tabla_trabajo %>% 
  left_join(hhi) %>% 
  left_join(data_101) %>% 
  mutate(across(c(utilidad_ejercicio_3420,
                  utilidad_gravable_3560,
                  vln_eaf_tdc_1800,
                  vln_eaf_tce_1810,
                  venta_neta_activos_fijos_1940,
                  exportaciones_netas_1820,
                  expor_netas_servi_1822,
                  costo_ssa_qcm_2280,
                  gasto_ssa_qcm_2290,
                  vnd_ssa_qcm_2295,
                  costo_bsi_qnc_2300,
                  gasto_bsi_qnc_2310,
                  vnd_bsi_qnc_2315,
                  costo_ies_2360,
                  gasto_ies_2370,
                  vnd_ies_2375,
                  cto_hon_pro_dietas_2380,
                  gto_hon_pro_dietas_2390,
                  vnd_hon_pro_dietas_2395,
                  cto_hon_pro_soc_2400,
                  gto_hon_pro_soc_2410,
                  vnd_hon_pro_soc_2415,
                  cto_provisiones_jpa_2730,
                  gto_provisiones_jpa_2740,
                  vpn_provisiones_jpa_2745,
                  cto_provisiones_desahucio_2750,
                  cto_otr_gto_ben_emp_2752,
                  gto_otr_gto_ben_emp_2754,
                  vnd_gto_otr_gto_ben_emp_2756,
                  gto_provisiones_desahucio_2760,
                  vnd_provisiones_desahucio_2765,
                  total_activo_1080,
                  total_patrimonio_neto_1780,
                  total_ingresos_1930,
                  totas_costos_gastos_3380,
                  impuesto_renta_causado_3600,
                  ctr_gen_anticipo_3613,
                  inversiones_corrientes_180,
                  inc_sub_aso_785,
                  inc_neg_con_795,
                  otr_inversiones_lpl_800,
                  utilidad_ejercicio_3420,
                  total_ingresos_1930),~replace_na(.x,0))) %>% 
  
  transmute(
    
    # Times of treatment and assignment groups
    
    treatment = as.numeric(dummy_nacional_101 == 0 & dummy_pf50_101 == 1 & dummy_ext50_101 == 0),
    control = as.numeric(dummy_nacional_101 == 0 & dummy_pf50_101 == 0 & dummy_ext50_101 == 1),
    pre_1 = as.numeric(anio_fiscal <= 2015),
    pre_2 = as.numeric(between(anio_fiscal,2013,2015)),
    post_1 = as.numeric(anio_fiscal > 2015),
    post_2 = as.numeric(between(anio_fiscal,2016,2018)),
    
    # Identifiers:
    
    anio_fiscal = anio_fiscal,
    identificacion_informante_anon = identificacion_informante_anon,
    
    section = section,
    # Shareholder relationships:
    hhi_th = hhi_th, # Shareholder HHI (tax haven)
    beneficiarios_total = beneficiarios_pff + beneficiarios_ext + beneficiarios_nac,
    share_pff = beneficiarios_pff/beneficiarios_total, # Number of taxhaven shareholders
    share_ext = beneficiarios_ext/beneficiarios_total, # Number of non taxhaven shareholders
    share_nac = beneficiarios_nac/beneficiarios_total, # Number of domestic shareholders
    porcentage_pff = porcentaje_pff, # Share of taxhaven shareholders
    porcentage_ext = porcentaje_ext, # Share of non taxhaven shareholders
    porcentage_nac = porcentaje_nac, # Share of domestic shareholders
    # Pre tax activity
    net_profits = utilidad_ejercicio_3420, # Profits
    taxable_profits = utilidad_gravable_3560, # Taxable profits
    local_sales = vln_eaf_tdc_1800 + vln_eaf_tce_1810 + venta_neta_activos_fijos_1940, # Local sales
    exports = exportaciones_netas_1820  + expor_netas_servi_1822, # Exports
    total_sales = local_sales + exports, # Total Sales
    labor_costs = costo_ssa_qcm_2280 + # Labor Costs
      gasto_ssa_qcm_2290 + 
      vnd_ssa_qcm_2295 + 
      costo_bsi_qnc_2300 + 
      gasto_bsi_qnc_2310 + 
      vnd_bsi_qnc_2315 + 
      costo_ies_2360 + 
      gasto_ies_2370 + 
      vnd_ies_2375 +
      cto_hon_pro_dietas_2380 + 
      gto_hon_pro_dietas_2390 + 
      vnd_hon_pro_dietas_2395 + 
      cto_hon_pro_soc_2400 + 
      gto_hon_pro_soc_2410 + 
      vnd_hon_pro_soc_2415 + 
      cto_provisiones_jpa_2730 + 
      gto_provisiones_jpa_2740 + 
      vpn_provisiones_jpa_2745 + 
      cto_provisiones_desahucio_2750 + 
      cto_otr_gto_ben_emp_2752 + 
      gto_otr_gto_ben_emp_2754 + 
      vnd_gto_otr_gto_ben_emp_2756 + 
      gto_provisiones_desahucio_2760 + 
      vnd_provisiones_desahucio_2765,
    total_assets = total_activo_1080, # Assets
    total_capital = total_patrimonio_neto_1780, # Capital
    total_income = total_ingresos_1930, # Total Income
    total_cost_expenses = totas_costos_gastos_3380,
    labor_cost_ratio = if_else(total_cost_expenses != 0,
                               labor_costs/total_cost_expenses,
                               0), # Importance of labor cost among overall cost
    
    # Tax liability:
    
    tax_liability = impuesto_renta_causado_3600,
    excess_anticipate = ctr_gen_anticipo_3613,
    
    # Investments:
    # Short term:
    
    current_investment = inversiones_corrientes_180,
    
    # Long term:
    
    long_term_inv_subasoc = inc_sub_aso_785 ,  
    long_term_inv_joint = inc_neg_con_795  ,   
    long_term_inv_other= otr_inversiones_lpl_800  ,
    
    # Profitability
    
    gross_profit_margin = utilidad_ejercicio_3420/total_sales,
    net_profit_margin = total_ingresos_1930/total_sales,
    profitability = utilidad_ejercicio_3420/total_ingresos_1930,
    return_on_assets = total_ingresos_1930/total_activo_1080,
    gross_profit_margin = case_when(gross_profit_margin > 1 | is.infinite(gross_profit_margin) ~ 1,
                                    is.nan(gross_profit_margin)  ~ 0,
                                    TRUE ~ gross_profit_margin),
    net_profit_margin = case_when(net_profit_margin > 1 | is.infinite(net_profit_margin) ~ 1,
                                  is.nan(net_profit_margin)  ~ 0,
                                  TRUE ~ net_profit_margin),
    profitability = case_when(profitability > 1 | is.infinite(profitability) ~ 1,
                              is.nan(profitability)  ~ 0,
                              TRUE ~ profitability),
    return_on_assets = case_when(return_on_assets > 1 | is.infinite(return_on_assets) ~ 1,
                                 is.nan(return_on_assets)  ~ 0,
                                 TRUE ~ return_on_assets),
    
    # Deliver of treatment
    tasa_ir = tasa_ir,
    deliver_treat = as.numeric(tasa_ir > tasa_vigente)
    
  )



# Add fixed effects -------------------------------------------------------

firm_size <- tabla_modelos %>% 
  filter(anio_fiscal <= 2015,
         total_income > 0) %>% 
  group_by(identificacion_informante_anon) %>% 
  summarise(ingresos_medios = mean(total_income,na.rm = T)) %>% 
  ungroup() 

firm_size <- firm_size %>% 
  mutate(size = cut(ingresos_medios,
                    breaks = quantile(firm_size$ingresos_medios,
                                      probs = seq(0,1,0.1),
                                      na.rm = TRUE)),
         size_factor = factor(as.numeric(size))) 


tabla_modelos <- tabla_modelos %>% 
  left_join(firm_size)


write_rds(tabla_modelos,file = "01_DATA/APS/tabla_modelos_aps_101.rds",compress = "gz")


collapse_mid_investments <- function(mid_year){
  
  mid_year %>% 
    filter(tipo_factor == "S",
           motivo_factor %in% c(405,410,415,
                                615,625,630,635,640,645,650,
                                710,720)) %>% 
    mutate(anio_fiscal = year(fecha_year)) %>% 
    group_by(id_an,
             anio_fiscal,
             pais,
             motivo_factor) %>% 
    summarise(monto = sum(monto,na.rm = T)) %>% 
    rename(identificacion_informante_anon = id_an)
  
}

mid_invest <- list.files("01_DATA/MID/completo/",full.names = T)[5:12] %>% 
  map(~{
    
    tabla <- read_rds(.x) %>% 
      mutate(monto = str_replace(monto,",","."),
             monto = as.numeric(monto))
    
    safely(collapse_mid_investments)(tabla)
    
  })


mid_invest_df <- mid_invest %>% 
  map("result") %>% 
  reduce(bind_rows)

iso_pais <- read_tsv("01_DATA/MID/iso_sri_country.txt")

paises_mid <- mid_invest_df %>% 
  ungroup() %>% 
  distinct(anio_fiscal,pais)

paises_mid <- paises_mid %>% 
  mutate(pais = replace_na(pais,"desconocido")) %>% 
  left_join(iso_pais,by = c("pais" = "iso_alpha2")) 

part_list_df <- read_rds("01_DATA/APS/lista_unicos_descollapsed.rds")

sample <- part_list_df %>% map(head) %>% reduce(bind_rows)

paises_aps <- part_list_df %>% 
  map(~.x %>% 
        distinct(anio_fiscal,pais_accionista_unico,paraiso_fiscal_accionista))

paises_aps <- paises_aps %>% 
  reduce(bind_rows) %>% 
  unique()

paraisos_fiscales <- paises_aps %>% 
  filter(paraiso_fiscal_accionista == "S")

par_unique <- paraisos_fiscales %>% 
  distinct(pais_accionista_unico)

mid_unique <- paises_mid %>% 
  distinct(pais,nombre_sri,nombre_iso)

temp <- tibble(
  pais = par_unique$pais_accionista_unico,
  pos = stringdist::amatch(par_unique$pais_accionista_unico,
                           table = iso_pais$nombre_iso,
                           method = "cosine")
)

temp$pais_match <- iso_pais$nombre_iso[temp$pos]

write_tsv(temp,"matches_aps_mid.txt")

library(readxl)

matched <- read_excel("match_correct.xlsx")

matched <- matched %>% 
  mutate(pais_match = if_else(pais == "ISLAS CAIMAN","ISLAS CAIMÁN",pais_match ))


paraisos_fiscales_mid_aps <- paraisos_fiscales %>% 
  left_join(matched,by = c("pais_accionista_unico" = "pais")) %>% 
  right_join(paises_mid, by = c("pais_match" = "nombre_iso",
                                "anio_fiscal"))

paraisos_fiscales_mid_aps %>% 
  write_rds("paises_mid_aps.rds")

join_paises_mid <- paraisos_fiscales_mid_aps %>% 
  filter(paraiso_fiscal_accionista == "S") %>% 
  select(anio_fiscal,pais,paraiso_fiscal_accionista)

mid_invest_df_row <- mid_invest_df %>% 
  left_join(join_paises_mid) %>% 
  ungroup() %>% 
  mutate(paraiso_fiscal_accionista = if_else(is.na(paraiso_fiscal_accionista),"ext","pff"),
         motivo_factor = str_c(paraiso_fiscal_accionista,motivo_factor,sep = "_") ) %>% 
  select(-paraiso_fiscal_accionista) %>% 
  group_by(identificacion_informante_anon,anio_fiscal,motivo_factor) %>% 
  summarise(monto = sum(monto,na.rm = T)) %>% 
  pivot_wider(names_from = motivo_factor,values_from = monto)

mid_invest_df_row <- mid_invest_df_row %>% 
  ungroup() %>% 
  mutate(across(where(is.numeric),~replace_na(.x,0)))

tabla_modelos <- tabla_modelos %>% 
  left_join(mid_invest_df_row)

write_rds(tabla_modelos,file = "01_DATA/APS/tabla_modelos_aps_101.rds",compress = "gz")


