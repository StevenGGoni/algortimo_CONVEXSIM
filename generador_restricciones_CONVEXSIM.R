
# Paquetes ----------------------------------------------------------------

library(tidyverse)

# cargar funciones --------------------------------------------------------

source("funcion_CONVEXSIM.R")
source("funcion_obtener_limites.R")

# Cargar restricciones EV generadas y almacenadas --------------------------

load("restricciones/restricciones_originales.RData")

# aplicación de CONVEXSIM -------------------------------------------------

restricciones_convexsim <- list(dos_dim, tres_dim, cuatro_dim, 
                                cinco_dim, seis_dim, siete_dim,
                                ocho_dim, nueve_dim, diez_dim, 
                                once_dim) %>%
  purrr::map(function(dimension){
    
    lista_I <- 1:nrow(dimension) %>%
      purrr::map(\(x) convexsim(dimension[x,], "I"))
    
    lista_S <- 1:nrow(dimension) %>%
      purrr::map(\(x) convexsim(dimension[x,], "S"))
    
    p <- length(lista_I[[1]][[1]])
    
    # CS = CONVEXSIM
    
    CS_I <- obtener_limites(lista_I) %>% 
      setNames(c(stringr::str_c("li_x", 1:p),
                 stringr::str_c("ls_x", 1:p),
                 "Región",
                 "Validador",
                 "CS")) %>% 
      purrr::modify_at(1:(2*p), as.numeric) %>% 
      cbind(dimension) %>% 
      dplyr::filter(CS == "I",
                    Región == "Correcto",
                    if_all(matches("ls_"), ~ . != 0))
    
    CS_S <- obtener_limites(lista_S) %>% 
      setNames(c(stringr::str_c("li_x", 1:p),
                 stringr::str_c("ls_x", 1:p),
                 "Región",
                 "Validador",
                 "CS")) %>% 
      purrr::modify_at(1:(2*p), as.numeric) %>%
      cbind(dimension) %>%
      dplyr::filter(CS == "S",
                    Región == "Correcto",
                    if_all(matches("ls_"), ~ . != 0))
    
    # Return
    
    list("I" = CS_I, "S" = CS_S)
    
  }) 

beepr::beep(3)

save(restricciones_convexsim,
     file = "restricciones/restricciones_convexsim.RData")

rm(list = ls()) # limpiar ambiente
gc() # garbage collector
