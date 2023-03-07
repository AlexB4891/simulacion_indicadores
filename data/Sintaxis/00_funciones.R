
# Functions ---------------------------------------------------------------


# Get the distributions between control and treatment groups --------------

prepare_data <- function(panel_data,
                         variable,
                         log,
                         winsorise){
  
  
  x_lab <- NULL
  
  winsorized <- NULL
  
  tabla <- panel_data %>% 
    filter(if_any(c(control,treatment),~.x == 1),
           anio_fiscal <= 2017) %>% 
    mutate(treatment = factor(treatment,
                              levels = c(0,1),
                              labels = c("Control","Treatment")))
  
  count_cero <- tabla %>% 
    filter(anio_fiscal < 2015) %>% 
    transmute(across(one_of(variable), 
                     list(ceros = ~.x <= 0),.names = "{.fn}")) %>% 
    count(ceros)
  
  if(winsorise){
    
    tabla <- tabla %>% 
      mutate(across(one_of(variable),
                    ~winsorize(.x,threshold = 0.01)))
    
    winsorized <- "Winsorized variable at 99th percentile."
    
  }
  
  if(log){
    
    postives <- count_cero %>% 
      filter(!isTRUE(ceros)) %>% 
      pull(n)
    
    ceros_n <- count_cero %>% 
      filter(ceros) %>% 
      pull(n)
    
    tabla <- tabla %>% 
      filter(if_any(one_of(variable),~.x > 0)) %>% 
      mutate(across(one_of(variable),
                    ~log(.x)))
    
    x_lab <- str_c("The variable has ",postives, " positive values and ", ceros_n, " values equal to 0.")
  }
  
  

  
  return(list(tabla = tabla,
              x_lab = x_lab,
              winsorized = winsorized))
}

compare_distributions <- function(panel_data,
                                  variable,
                                  label,
                                  panel_title,
                                  log,
                                  winsorise){
  
  
  data_d <- panel_data %>% 
    filter(anio_fiscal < 2015) %>% 
    group_by(identificacion_informante_anon,
             treatment) %>% 
    summarise(across(variable,
                     mean,
                     na.rm = T)) %>% 
    ungroup() 
  
  
  datos_prueba <- data_d %>% 
    arrange(treatment) %>% 
    group_by(treatment) %>% 
    summarise(n = n_distinct(identificacion_informante_anon),
              across(variable,
                     list(mean = mean,
                          sd = sd),
                     na.rm = T,
                     .names = "{.fn}"))
  
  
  ns <- datos_prueba %>% 
    pull(n)
  
  medias <- datos_prueba %>% 
    pull(mean)
  
  diff <- sum(medias*c(1,-1))
  
  
  sd <- datos_prueba %>% 
    pull(sd)
  
  sd <- sd^2
  
  se <- sqrt(sum(sd/ns))
  
  intervalo <- c(
    diff - 1.96*se,
    diff + 1.96*se
  )
  
  test <- between(0,
                  intervalo[1],
                  intervalo[2])
  
  capti <- str_c(
    "The mean for the control group is ", round(medias[1],3)," (N = ",ns[1],") and the mean for the ",
    "treatment group is ",round(medias[2],3)," (N = ",ns[2],"). ",
    "The confidence interval for the mean difference between both groups ",
    "is [",round(intervalo[1],3),", ",round(intervalo[2],3),"]. ",
    "Missing values are droped from the distribution.")
  
  
  if(isTRUE(test)){
    capti <- str_c(capti," Since 0 is inside the confidence intervalo, there is sufficient evidence to accept the null hypothesis, the difference of the means is equal to 0.")
  }else{
    capti <- str_c(capti," Since 0 is outside the confidence intervalo, there is sufficient evidence to reject the null hypothesis, the difference of the means is not equal to 0.")
  }
  
  x_lab <- label
  
  if(!is.null(log)){
    
    x_lab = str_c("log(",label,")")
    
    capti <- str_c(log," ",capti)
    
  }
  
  if(!is.null(winsorise)){
    capti <- str_c(winsorise," ",capti)
  }
  

  
  data_d %>% 
    ggplot(mapping = aes_string(x = variable)) +
    geom_histogram(aes(fill = treatment),
                   alpha = 0.5,
                   position = "identity") +
    scale_fill_manual(values = c("Control" = "#FF5733",
                                  "Treatment" = "#00C18F")) +
    theme_light() +
    labs(y = "Firms",
         x = x_lab,
         fill = "Group",
         caption = str_wrap(capti,120),
         subtitle = panel_title) +
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0.5,
                                      size = 8)) 
}


summarize_table_function <- function(tabla,
                                     variable){
  
  data_d <- tabla %>% 
    filter(anio_fiscal < 2015) %>% 
    group_by(identificacion_informante_anon,
             treatment) %>% 
    summarise(across(variable,
                     mean,
                     na.rm = T)) %>% 
    ungroup() 
  
  data_d %>% 
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
                 values_to = variable) %>% 
    mutate(across(one_of(variable),round,digits = 3))
  
}


puntual_diff_diff <- function(tabla, 
                              variable){
  
  mean_df <- tabla %>% 
    filter(anio_fiscal < 2015) %>% 
    group_by(identificacion_informante_anon,
             treatment) %>% 
    summarise(across(.cols = one_of(variable),
                     ~mean(.x,na.rm = T))) %>% 
    ungroup() %>% 
    group_by(treatment)  %>% 
    summarise(across(.cols = one_of(variable),
                     ~mean(.x,na.rm = T))) %>% 
    rename_with(.cols = one_of(variable),
                ~"mean_1214")
  
  yearly_df <- tabla %>% 
    group_by(anio_fiscal,
             treatment) %>% 
    summarise(across(.cols = one_of(variable),
                     ~mean(.x,na.rm = T))) %>% 
    rename_with(.cols = one_of(variable),
                ~"mean_year")
  
  result_df <- inner_join(mean_df,
                          yearly_df) %>% 
    mutate(ratio = mean_year/mean_1214)
  
  
  return(table = result_df)
  
}



diff_diff_estimation <- function(tabla,variable){

  mean_df_ind <- tabla %>%
    filter(anio_fiscal < 2014) %>%
    group_by(treatment,
             identificacion_informante_anon)  %>%
    summarise(across(.cols = one_of(variable),
                     ~mean(.x,na.rm = T))) %>%
    rename_with(.cols = one_of(variable),
                ~"mean_1214")

  yearly_df_ind <- tabla %>%
    filter(anio_fiscal < 2018) %>% 
    group_by(anio_fiscal,
             identificacion_informante_anon,
             treatment) %>%
    summarise(across(.cols = one_of(variable),
                     ~mean(.x,na.rm = T))) %>%
    rename_with(.cols = one_of(variable),
                ~"mean_year")


  result_df_ind <- inner_join(mean_df_ind,
                              yearly_df_ind) %>%
    mutate(ratio = case_when(mean_1214 == 0 ~ 1,
                             is.na(mean_1214) ~ 1,
                             TRUE ~ mean_year/mean_1214),
           pre_post = if_else(anio_fiscal <= 2015,"pre","post"),
           pre_post = factor(pre_post,levels = c("pre","post")))



  modelo <- feols(data = result_df_ind %>% filter(!is.nan(ratio),
                                               !is.na(ratio),
                                               !is.infinite(ratio)),
               fml = ratio ~ treatment + pre_post +  treatment*pre_post,
               fixef  = c("anio_fiscal",
                          "identificacion_informante_anon"))
  
  modelo_df <- tidy(modelo, conf.int = TRUE)

  performance <- glance(modelo)


  return(list(
              model_estimation = modelo_df,
              performance = performance))

}


plot_function <- function(tabla,
                          label,
                          panel_title){
  
  tabla %>% 
    ggplot() +
    geom_line(aes(x = anio_fiscal,
                  y = ratio,
                  color = treatment)) +
    geom_point(aes(x = anio_fiscal,
                   y = ratio,
                   color = treatment,
                   shape = treatment)) +
    scale_color_manual(values = c("Control" = "#FF5733",
                                  "Treatment" = "#00C18F")) +
    scale_shape_manual(values = c(4,16)) +
    theme_minimal() +
    labs(y = str_c(label,"\n(Relative to 2012-2014 mean)"),
         x = "Fiscal year" ,
         color = "Group",
         shape = "Group",
         subtitle = panel_title) +
    theme(legend.position = "bottom")
}




generate_all_summaries <- function(panel_data,
                                   variable,
                                   label,
                                   panel_title,
                                   log,
                                   winsorise){
  
  
  temp_list <- prepare_data(panel_data,
                                variable,
                                log,
                                winsorise)
  
  data_d <- temp_list$tabla
  
  log_label <- temp_list$x_lab
  
  win_label <- temp_list$winsorized
  
  # browser()
  
  if(nrow(data_d) > 0){
  
  plot_distribution <- compare_distributions(panel_data = data_d,
                                             variable = variable,
                                             label = label,
                                             panel_title = panel_title,
                                             log = log_label,
                                             winsorise = win_label)

  latex_table <- summarize_table_function(tabla = data_d,
                                          variable = variable)
  
  tabla_diff_diff <- puntual_diff_diff(tabla = data_d,
                    variable = variable)
  
  plot_diff <- plot_function(tabla = tabla_diff_diff,
                label = label,
                panel_title = panel_title)
  # 
  # estimation <- diff_diff_estimation(tabla = data_d,
  #                                    variable = variable)
  
  return(list(
    plot_distribution,
    latex_table,
    tabla_diff_diff,
    plot_diff
    # ,
    # estimation
    ))
  
  }else{
    
    NULL
  }
  
  
}


