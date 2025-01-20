
# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(mixexp) 


# generación de restricciones ---------------------------------------------

## Tres y cuatro componentes ----

x1 <- x2 <- x3 <- x4 <- seq(0.1, 1, by = 0.05)

dos_dim <- tidyr::expand_grid(x1, x2, x3) %>% 
  dplyr::mutate(EV = rowSums(.[1:3]) - min(.[1:3])) %>% 
  dplyr::filter(EV > 1) %>% # solo aquellos diseños que tengan sentido
  dplyr::select(-EV)

tres_dim <- tidyr::expand_grid(x1, x2, x3, x4) %>% 
  dplyr::mutate(EV = rowSums(.[1:4]) - min(.[1:4])) %>% 
  dplyr::filter(EV > 1) %>% 
  dplyr::select(-EV)

## Cinco y seis componentes ----

x1 <- x2 <- x3 <- x4 <- x5 <- x6 <- seq(0.1, 1, by = 0.1)

cuatro_dim <- tidyr::expand_grid(x1, x2, x3, x4, x5) %>% 
  dplyr::mutate(EV = rowSums(.[1:5]) - min(.[1:5])) %>% 
  dplyr::filter(EV > 1) %>% 
  dplyr::select(-EV)

cinco_dim <- tidyr::expand_grid(x1, x2, x3, x4, x5, x6) %>% 
  dplyr::mutate(EV = rowSums(.[1:6]) - min(.[1:6])) %>% 
  dplyr::filter(EV > 1) %>% 
  dplyr::select(-EV)

## Siete y ocho componentes ----

x1 <- x2 <- x3 <- x4 <- x5 <- x6 <- x7 <- x8 <- seq(0.1, 1, by = 0.15)

seis_dim <- tidyr::expand_grid(x1, x2, x3, x4, x5, x6, x7) %>% 
  dplyr::mutate(EV = rowSums(.[1:7]) - min(.[1:7])) %>% 
  dplyr::filter(EV > 1) %>% 
  dplyr::select(-EV)

siete_dim <- tidyr::expand_grid(x1, x2, x3, x4, x5, x6, x7, x8) %>% 
  dplyr::mutate(EV = rowSums(.[1:8]) - min(.[1:8])) %>% 
  dplyr::filter(EV > 1) %>% 
  dplyr::select(-EV)

## Nueve y diez componentes ----

x1 <- x2 <- x3 <- x4 <- x5 <- x6 <- 
  x7 <- x8 <- x9 <- x10 <- seq(0.1, 1, by = 0.20)


ocho_dim <- tidyr::expand_grid(x1, x2, x3, x4,
                               x5, x6, x7, x8, x9) %>% 
  dplyr::mutate(EV = rowSums(.[1:9]) - min(.[1:9])) %>% 
  dplyr::filter(EV > 1) %>% 
  dplyr::select(-EV)

nueve_dim <- tidyr::expand_grid(x1, x2, x3, x4, x5, 
                                x6, x7, x8, x9, x10) %>% 
  dplyr::mutate(EV = rowSums(.[1:10]) - min(.[1:10])) %>% 
  dplyr::filter(EV > 1) %>% 
  dplyr::select(-EV)

## Once y doce componentes ----

x1 <- x2 <- x3 <- x4 <- x5 <- x6 <- x7 <- 
  x8 <- x9 <- x10 <- x11 <- x12 <- seq(0.1, 1, by = 0.25)


diez_dim <- tidyr::expand_grid(x1, x2, x3, x4,
                               x5, x6, x7, x8, x9, x10, x11) %>% 
  dplyr::mutate(EV = rowSums(.[1:11]) - min(.[1:11])) %>% 
  dplyr::filter(EV > 1) %>% 
  dplyr::select(-EV)

once_dim <- tidyr::expand_grid(x1, x2, x3, x4, x5, x6, 
                               x7, x8, x9, x10, x11,x12) %>% 
  dplyr::mutate(EV = rowSums(.[1:12]) - min(.[1:12])) %>% 
  dplyr::filter(EV > 1) %>% 
  dplyr::select(-EV)


# Guardar restriciones generadas ------------------------------------------

## Esto simplifica el trabajo de no tener que volver a correr 
## el código anterior, que puede ser algo demandante. 

save(dos_dim, tres_dim, cuatro_dim, cinco_dim, seis_dim, 
     siete_dim, ocho_dim, nueve_dim, diez_dim, once_dim,
     file = "restricciones/restricciones_originales.RData")

rm(list = ls()) # limpiar ambiente
gc() # garbage collector

# Cargar restricciones generadas y almacenadas ----------------------------

load("restricciones/restricciones_originales.RData")

# cargar funciones --------------------------------------------------------

source("funcion_CONVEXSIM.R")
source("funcion_obtener_limites.R")

# aplicación de CONVEXSIM -------------------------------------------------

restricciones_convexsim <- list(dos_dim, tres_dim) %>%  
                                # cuatro_dim) %>% 
                                # , cinco_dim) %>% 
                                # seis_dim, siete_dim,
                                # ocho_dim, nueve_dim,
                                # diez_dim, once_dim) %>%
  purrr::map(function(dimension){
    
    lista_I <- 1:nrow(dimension) %>%
      purrr::map(\(x) convexsim(dimension[x,], "I"))
    
    lista_S <- 1:nrow(dimension) %>%
      purrr::map(\(x) convexsim(dimension[x,], "S"))
    
    # CS = CONVEXSIM
    
    CS_I <- obtener_limites(lista_I)
    
    CS_S <- obtener_limites(lista_S)
    
    # Return
    
    list("I" = CS_I, "S" = CS_S)
    
  }) 

beepr::beep(3)

# generación de diseños ---------------------------------------------------

convexsim(ls = data.frame(x1 = 0.75,
                          x2 = 0.75,
                          x3 = 0.75,
                          x4 = 0.75,
                          x5 = 0.75, 
                          x6 = 0.75))


# pendiente ---------------------------------------------------------------

combinaciones <- function(dde) {
  # Get column names
  col_names <- names(dde)
  
  # Generate all pair combinations
  comb <- combn(col_names, 2, simplify = FALSE)
  
  # Use purrr::map to iterate over each pair and create new columns
  multiplicacion <- map(comb, ~ {
    col1 <- .x[1]
    col2 <- .x[2]
    new_col_name <- paste(col1, col2, sep = "_x_")
    data %>%
      transmute(!!new_col_name := .[[col1]] * .[[col2]])
  })
  
  bind_cols(data, bind_cols(multiplicacion)) 
}


# zona de pruebas (borrar) ------------------------------------------------

test <- 1:2 %>%
  purrr::map(\(x) convexsim(cuatro_dim[x,], "I"))
# 
# test_li <- 1:length(test) %>% 
#   purrr::map(\(x) t(as.data.frame(test[[x]][1]))) %>% 
#   purrr::reduce(rbind.data.frame) %>% 
#   setNames(stringr::str_c("li_x", 1:5))


purrr::map(1:4, ~replicar(final_test, .x))

p <- 6

dde <- mixexp::Xvert(nfac = p, 
                     uc = rep(0.75, p), #as.numeric(dos_dim[800,]),
                     lc = rep(0.05, p), 
                     ndm = 0, # 0 --> m = 1
                     pseudo = FALSE, 
                     plot = TRUE) %>% 
  signif(3)


dos_dim[1,-(p + 1)] %>% as.numeric()


1:nrow(test) %>% 
  purrr::map_df(function(x){
    
    (dos_dim[1,] - test[x, -ncol(test)])/(sum(dos_dim[1,])-1)
    
  })


test <- generar_diseno_SR(p =4, m = 3, ax = T, n = 1)

test2 <- generar_diseno_EV(p = 3, m =2, ax = T, ls = dos_dim[1, ], n = 1)

mixexp::DesignPoints(dde)
mixexp::DesignPoints(test2$Proporciones)

mixexp::Xvert(nfac = 3, 
              uc = rep(1, 3), 
              lc = rep(0, 3), 
              ndm = 2, # 0 --> m = 1
              plot = T,
              pseudo = FALSE)

mixexp::SLD(3, 2) %>% mixexp::DesignPoints()

mixexp::SCD(3) %>% mixexp::DesignPoints()

mixexp::crvtave()

test3 <- names(mixexp::SLD(3, 1))

centro <- as.data.frame(t(rep(1/3, 3))) %>% 
  setNames(test3)

mixexp::SLD(3, 1) %>% rbind.data.frame(.,centro)
crear_axiales()

dde <- mixexp::Xvert(nfac = n, 
                     uc = ls, 
                     lc = rep(0, n), 
                     ndm = m, # 0 --> m = 1
                     plot = FALSE,
                     pseudo = FALSE) %>% 
  signif(3)


1:10 |>
  map(\(x) rnorm(10, x))
