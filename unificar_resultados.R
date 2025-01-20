
# libraries ---------------------------------------------------------------

library(tidyverse)

# cargar datos ------------------------------------------------------------

load("resultados/resultados3.RData")

tres_componentes <- preliminar

load("resultados/resultados4.RData")

cuatro_componentes <- preliminar

load("resultados/resultados5.RData")

cinco_componentes <- preliminar

load("resultados/resultados6.RData")

seis_componentes <- preliminar[, -c(75:79)]

load("resultados/resultados7.RData")

siete_componentes <- preliminar

load("resultados/resultados8.RData")

ocho_componentes <- preliminar

load("resultados/resultados9.RData")

nueve_componentes <- preliminar[, -c(84:88)] %>% 
  tidyr::drop_na(D_opt_EV_Prop)

load("resultados/resultados10.RData")

diez_componentes <- preliminar[, -c(87:91)] %>% 
  tidyr::drop_na(D_opt_EV_Prop)

rm(preliminar)

# unificar datos ----------------------------------------------------------

resultados_unificados <- list(tres_componentes, 
                              cuatro_componentes,
                              cinco_componentes,
                              seis_componentes, 
                              siete_componentes, 
                              ocho_componentes,
                              nueve_componentes, 
                              diez_componentes) %>%
  purrr::map_dfr(function(x){
    
    x %>% 
      dplyr::mutate(across(matches("V.*[0-9]"), \(x) round(x, 2))) %>% 
      dplyr::rowwise() %>%
      dplyr::mutate(n_b_i = sum(c_across(matches("V.*[0-9]")) != 1.0)) %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(tam_rest = R_prom_ev*p) %>% 
      dplyr::select(-matches("V.*[0-9]"), 
                    -matches("li_"),
                    -matches("ls_")) %>% 
      dplyr::select(p, n_b_i, tam_rest, m, axiales, n, everything())
    
  }) %>% 
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 4)))

save(resultados_unificados, 
     file = "resultados/resultados_unificados.RData")

rm(list = ls())
