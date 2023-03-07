library(tidyverse)
library(gt)
library(readxl)
library(janitor)


# Tabla de summaries ------------------------------------------------------



tabla_resumen <- read_tsv("data/resumenes_outcomes_ejercicio.txt")


# Diff in diff tables -----------------------------------------------------


tabla_diff_diff <- read_tsv("data/tabla_graficos.txt")
tabla_diff_diff_model <- read_tsv("data/tabla_modelos.txt")
tabla_diff_diff_perfo <- read_tsv("data/tabla_performance.txt")


# Diccionario -------------------------------------------------------------


diccionario <- read_excel("data/database_dictionary.xlsx",skip = 8)

diccionario <- diccionario %>% 
  clean_names()

names(tabla_resumen)


# Función tabla -----------------------------------------------------------



make_latex_table <- function(summ_table,
                             variables){
  
  
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
                                     "(",1:nrow(labels_df),")"))
  
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


# Generar tabla summary ---------------------------------------------------


make_latex_table(tabla_resumen,
                 c( "hhi_th","beneficiarios_total",
                    "share_pff","share_ext"  )) %>% 
  tab_options(table.width = px(1000)) %>% 
  gt::gtsave("tabla_1.png",
             vwidth = 1700,
             vheight = 1200) 
  


diff_diff_table <- function(tabla,panel_text){

  tabla %>% 
    filter(panel == panel_text) %>% 
    mutate(pre_post = if_else(anio_fiscal <= 2015,"pre","post"),
           pre_post = factor(pre_post,levels = c("pre","post"))) %>% 
    group_by(pre_post,treatment) %>% 
    summarise(ratio = mean(ratio)) %>% 
    pivot_wider(names_from = treatment,values_from = ratio) %>% 
    mutate(difference = treatment - control)
}


extract_broom <- function(tidy_model, glance_model) {
  
  # get estimates/standard errors from tidy
  coef <- tidy_model$estimate
  coef.names <- as.character(tidy_model$term)
  se <- tidy_model$std.error
  pvalues <- tidy_model$p.value
  # get goodness-of-fit statistics from glance
  glance_transposed <- as_tibble(cbind(name = names(glance_model), t(glance_model)))
  gof.names <- as.character(glance_transposed$name)
  gof <- as.double(glance_transposed$V2)
  gof.decimal <- gof %% 1 > 0
  tr_object <- texreg::createTexreg(coef.names = coef.names,
                                    coef = coef,
                                    se = se,
                                    pvalues = pvalues,
                                    gof.names = gof.names,
                                    gof = gof,
                                    gof.decimal = gof.decimal)
  return(tr_object)
}


# Manipular las tablas para el formato ------------------------------------


tabla_diff_diff_model <- tabla_diff_diff_model %>% 
  filter(panel != "todas_empresas") %>% 
  mutate(panel = factor(panel,
                           levels = c("panel_desbalanceado",
                                      "panel_balanceado",
                                      "panel_semibalanceado",
                                      "ingreso_positivo",
                                      "tasa_positiva"),
                           labels = c("Panel 1",
                                      "Panel 2",
                                      "Panel 3",
                                      "Panel 4",
                                      "Panel 5")
  ),
  term = factor(term,
                levels = c("(Intercept)",
                           "pre_postpost",                   
                           "treatmenttreatment" ,
                           "treatmenttreatment:pre_postpost"),
                labels = c("Intercept",
                           "Post",
                           "Treatment",
                           "Treatment:Post")
  ))


tabla_diff_diff_perfo <- tabla_diff_diff_perfo %>% 
  filter(panel != "todas_empresas") %>% 
  mutate(panel = factor(panel,
                        levels = c("panel_desbalanceado",
                                   "panel_balanceado",
                                   "panel_semibalanceado",
                                   "ingreso_positivo",
                                   "tasa_positiva"),
                        labels = c("Panel 1",
                                   "Panel 2",
                                   "Panel 3",
                                   "Panel 4",
                                   "Panel 5")
  )) %>% 
  rename_with(~c("R Squared",
                 "Adj. R Squared",
                 "Sigma",
                 "Statistic",
                 "P-Value",
                 "DF",
                 "LogLik",
                 "AIC",
                 "BIC",
                 "Deviance",
                 "DF Residual",
                 "N Obs.",
                 "Panel"))
  

# Generar latex Diff in Diff -----------------------------------------------------------

  
tabla_diff_diff_model <- tabla_diff_diff_model %>% 
  split(.$panel)

tabla_diff_diff_perfo <- tabla_diff_diff_perfo %>% 
  split(.$Panel)

tabla_diff_diff_perfo <- tabla_diff_diff_perfo %>% 
  map(select,-Panel)


list_c <- map2(
  .x = tabla_diff_diff_model,
  .y = tabla_diff_diff_perfo,
  extract_broom
)



texreg::texreg(list_c)


# Estimación puntual de diff in diff --------------------------------------

diff_diff_table(tabla_diff_diff,
                "todas_empresas") 


# Graficos ----------------------------------------------------------------


diff_diff_plot <- function(tabla,panel_text){
  
  tabla <- tabla %>% 
    filter(panel != "todas_empresas") %>% 
    filter(panel == panel_text) %>% 
    mutate(panel = factor(panel,
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
    treatment = str_to_sentence(treatment)) 
  
  tabla %>% 
    ggplot() +
    geom_line(aes(x = anio_fiscal,
                  y = ratio,
                  color = treatment)) +
    geom_point(aes(x = anio_fiscal,
                   y = ratio,
                   color = treatment,
                   shape = treatment)) +
    geom_vline(aes(xintercept = 2015),
               linetype = "dashed") +
    scale_shape_manual(values = c(4,16)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(subtitle = unique(tabla$panel),
         y = "Estimated tax rate (Relative to Pre'Reform Mean)",
         color = "",
         shape = "",
         x = "Fiscal year")
  }


graficos <- tabla_diff_diff %>% 
  filter(panel != "todas_empresas") %>% 
  pull(panel) %>% 
  unique %>% 
  map(
    ~diff_diff_plot(tabla_diff_diff,
                     panel_text = .x)
  )

graficos[[5]]
  

