# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(mixexp) 
library(gtools)

# generación de restricciones ---------------------------------------------

## Tres componentes ----

niveles <- seq(0.1, 1, by = 0.05)

dos_dim <- gtools::combinations(n = length(niveles),
                                r = 3,
                                v = niveles, 
                                repeats.allowed = TRUE) %>%
  as.data.frame() %>% 
  dplyr::mutate(EV = rowSums(across(everything())) - min(across(everything())),
                R_b = rowSums(across(-EV)) - 1, 
                alcanzable = rowSums(across(-c(EV, R_b)) > R_b)) %>% 
  dplyr::filter(EV > 1, # solo aquellos diseños que tengan sentido
                alcanzable == 0) %>% 
  dplyr::select(-c(EV, alcanzable, R_b)) %>% 
  dplyr::slice(1:(n()-1))

## Cuatro y Cinco componentes ----

niveles <- seq(0.1, 1, by = 0.1)

tres_dim <- gtools::combinations(n = length(niveles),
                                 r = 4,
                                 v = niveles, 
                                 repeats.allowed = TRUE) %>%
  as.data.frame() %>% 
  dplyr::mutate(EV = rowSums(across(everything())) - min(across(everything())),
                R_b = rowSums(across(-EV)) - 1, 
                alcanzable = rowSums(across(-c(EV, R_b)) > R_b)) %>% 
  dplyr::filter(EV > 1, # solo aquellos diseños que tengan sentido
                alcanzable == 0) %>% 
  dplyr::select(-c(EV, alcanzable, R_b)) %>% 
  dplyr::slice(1:(n()-1))

niveles <- seq(0.1, 1, by = 0.15)

cuatro_dim <- gtools::combinations(n = length(niveles),
                                   r = 5,
                                   v = niveles, 
                                   repeats.allowed = TRUE) %>%
  as.data.frame() %>% 
  dplyr::mutate(EV = rowSums(across(everything())) - min(across(everything())),
                R_b = rowSums(across(-EV)) - 1, 
                alcanzable = rowSums(across(-c(EV, R_b)) > R_b)) %>% 
  dplyr::filter(EV > 1, # solo aquellos diseños que tengan sentido
                alcanzable == 0) %>% 
  dplyr::select(-c(EV, alcanzable, R_b)) %>% 
  dplyr::slice(1:(n()-1))

## Seis componentes ----

niveles <- seq(0.1, 1, by = 0.15)

cinco_dim <- gtools::combinations(n = length(niveles),
                                  r = 6,
                                  v = niveles, 
                                  repeats.allowed = TRUE) %>%
  as.data.frame() %>% 
  dplyr::mutate(EV = rowSums(across(everything())) - min(across(everything())),
                R_b = rowSums(across(-EV)) - 1, 
                alcanzable = rowSums(across(-c(EV, R_b)) > R_b)) %>% 
  dplyr::filter(EV > 1, # solo aquellos diseños que tengan sentido
                alcanzable == 0) %>% 
  dplyr::select(-c(EV, alcanzable, R_b)) %>% 
  dplyr::slice(1:(n()-1))

## Siete componentes -----

niveles <- c(seq(0.1, 1, by = 0.25), 1)

seis_dim <- gtools::combinations(n = length(niveles),
                                 r = 7,
                                 v = niveles, 
                                 repeats.allowed = TRUE) %>%
  as.data.frame() %>% 
  dplyr::mutate(EV = rowSums(across(everything())) - min(across(everything())),
                R_b = rowSums(across(-EV)) - 1, 
                alcanzable = rowSums(across(-c(EV, R_b)) > R_b)) %>% 
  dplyr::filter(EV > 1, # solo aquellos diseños que tengan sentido
                alcanzable == 0) %>% 
  dplyr::select(-c(EV, alcanzable, R_b)) %>% 
  dplyr::slice(1:(n()-1))

## ocho y Nueve y diez componentes ----

niveles <- c(seq(0.1, 1, by = 0.40), 1)

siete_dim <- gtools::combinations(n = length(niveles),
                                  r = 8,
                                  v = niveles, 
                                  repeats.allowed = TRUE) %>%
  as.data.frame() %>% 
  dplyr::mutate(EV = rowSums(across(everything())) - min(across(everything())),
                R_b = rowSums(across(-EV)) - 1, 
                alcanzable = rowSums(across(-c(EV, R_b)) > R_b)) %>% 
  dplyr::filter(EV > 1, # solo aquellos diseños que tengan sentido
                alcanzable == 0) %>% 
  dplyr::select(-c(EV, alcanzable, R_b)) %>% 
  dplyr::slice(1:(n()-1))

ocho_dim <- gtools::combinations(n = length(niveles),
                                 r = 9,
                                 v = niveles, 
                                 repeats.allowed = TRUE) %>%
  as.data.frame() %>% 
  dplyr::mutate(EV = rowSums(across(everything())) - min(across(everything())),
                R_b = rowSums(across(-EV)) - 1, 
                alcanzable = rowSums(across(-c(EV, R_b)) > R_b)) %>% 
  dplyr::filter(EV > 1, # solo aquellos diseños que tengan sentido
                alcanzable == 0) %>% 
  dplyr::select(-c(EV, alcanzable, R_b)) %>% 
  dplyr::slice(1:(n()-1))

nueve_dim <- gtools::combinations(n = length(niveles),
                                  r = 10,
                                  v = niveles, 
                                  repeats.allowed = TRUE) %>%
  as.data.frame() %>% 
  dplyr::mutate(EV = rowSums(across(everything())) - min(across(everything())),
                R_b = rowSums(across(-EV)) - 1, 
                alcanzable = rowSums(across(-c(EV, R_b)) > R_b)) %>% 
  dplyr::filter(EV > 1, # solo aquellos diseños que tengan sentido
                alcanzable == 0) %>% 
  dplyr::select(-c(EV, alcanzable, R_b)) %>% 
  dplyr::slice(1:(n()-1))

## Once y doce componentes ----

niveles <- c(seq(0.1, 1, by = 0.40), 1)

diez_dim <- gtools::combinations(n = length(niveles),
                                 r = 11,
                                 v = niveles, 
                                 repeats.allowed = TRUE) %>%
  as.data.frame() %>% 
  dplyr::mutate(EV = rowSums(across(everything())) - min(across(everything())),
                R_b = rowSums(across(-EV)) - 1, 
                alcanzable = rowSums(across(-c(EV, R_b)) > R_b)) %>% 
  dplyr::filter(EV > 1, # solo aquellos diseños que tengan sentido
                alcanzable == 0) %>% 
  dplyr::select(-c(EV, alcanzable, R_b)) %>% 
  dplyr::slice(1:(n()-1))

once_dim <- gtools::combinations(n = length(niveles),
                                 r = 12,
                                 v = niveles, 
                                 repeats.allowed = TRUE) %>%
  as.data.frame() %>% 
  dplyr::mutate(EV = rowSums(across(everything())) - min(across(everything())),
                R_b = rowSums(across(-EV)) - 1, 
                alcanzable = rowSums(across(-c(EV, R_b)) > R_b)) %>% 
  dplyr::filter(EV > 1, # solo aquellos diseños que tengan sentido
                alcanzable == 0) %>% 
  dplyr::select(-c(EV, alcanzable, R_b)) %>% 
  dplyr::slice(1:(n()-1))


# Guardar restriciones generadas ------------------------------------------

## Esto simplifica el trabajo de no tener que volver a correr 
## el código anterior, que puede ser algo demandante. 

save(dos_dim, tres_dim, cuatro_dim, cinco_dim, seis_dim, 
     siete_dim, ocho_dim, nueve_dim, diez_dim, once_dim,
     file = "restricciones/restricciones_originales.RData")

rm(list = ls()) # limpiar ambiente
gc() # garbage collector
