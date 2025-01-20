
# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(mixexp) 
library(matlib)

# cargar datos de restricciones CONVEXSIM ---------------------------------

load("restricciones/restricciones_convexsim.RData")

# cargar funciones --------------------------------------------------------

source("funcion_generadora_SR.R")
source("funcion_generadora_VE.R")
source("funcion_generar_combinaciones.R")
source("funcion_Optimalidad.R")

# Variables de entrada restantes para la simulación -----------------------

variables <- tidyr::expand_grid(axiales = c(TRUE, FALSE),
                                m = 1:3, # grado del polinomio
                                n = 1:4) # réplicas totales

restricciones <- 1:10 %>% 
  purrr::map(function(x){
    
    restricciones_general <- rbind(restricciones_convexsim[[x]][["I"]],
                                   restricciones_convexsim[[x]][["S"]])
    
    resultado <- purrr::map_dfr(1:nrow(variables), ~ restricciones_general) %>% 
      dplyr::bind_cols(., variables[rep(1:nrow(variables), 
                                        each = nrow(restricciones_general)), ])
    
  })

rm(variables, restricciones_convexsim)
gc()

# Variables de salida -----------------------------------------------------

resultados <- restricciones[7] %>% 
  purrr::map(function(df){
    
    filas <- nrow(df)
    
    preliminar <- c(1153:1728, 2881:3456) %>%
      purrr::map_dfr(function(x){
        
        if(x %% 20 == 0){
          
          print(x)
          
        }
        
        m <- df[x, "m"] # grado del modelo
        
        n <- df[x, "n"] # réplicas
        
        ax <- df[x, "axiales"] # axiales
        
        li <- df[x, ] %>% # límite inferior
          dplyr::select(dplyr::matches("li_"))
        
        ls <- df[x, ] %>% # límite superior
          dplyr::select(dplyr::matches("ls_"))
        
        ls_EV <- df[x, ] %>% # límite superior EV
          dplyr::select(dplyr::matches("V.*[0-9]"))
        
        p <- length(li) # cantidad de componentes
        
        # Diseños --------
        
        ## Vértices extremos
        
        dde_ev <- try(generar_diseno_EV(p = p, 
                                        m = m, 
                                        ax = ax,
                                        li = rep(0, p), 
                                        ls = ls_EV,
                                        n = n))
        
        ## Vertices extremos convexsim 
        
        dde_conv <- try(generar_diseno_EV(p = p, 
                                          m = m, 
                                          ax = ax,
                                          li = li, 
                                          ls = ls,
                                          n = n))
        ## Símplex reticular
        
        dde_SR <- generar_diseno_SR(p = p, 
                                    m = m, 
                                    ax = ax, 
                                    n = n)
        
        if(is.list(dde_ev) | is.list(dde_conv)){
          
          # Cantidad de corridas por diseño -----
          
          ## Cantidad de corridas EV
          
          n_ev <- tryCatch(expr = nrow(dde_ev$Proporciones), 
                           error = function(e) 9999999)
          
          ## Cantidad de corridas EV - CONVEXSIM
          
          n_conv <- nrow(dde_conv$Proporciones)
          
          ## Cantidad de corridas simplex
          
          n_SR <- nrow(dde_SR$Proporciones)
          
          # Variables de salida -----
          
          ## Cantidad de corridas totales
          
          tn_SR <- (n_SR - n_conv)/n_SR ### Comparación contra SR
          
          tn_EV <- (n_ev - n_conv)/n_ev ### Comparación contra EV
          
          ## Distancia entre centroides
          
          dE_SR <- sqrt(sum((dde_SR$Centroide - dde_conv$Centroide)^2)) # Contra SR
          
          dE_EV <- tryCatch(expr = 
                              sqrt(sum((dde_ev$Centroide - dde_conv$Centroide)^2)), # Contra EV
                            error = function(e) 9999999)
          
          ## Tamaño de la región de experimentación 
          
          R_prom_ev <- mean(as.matrix(ls_EV))
          
          R_prom_con <- mean(as.matrix(ls) - as.matrix(li))
          
          R <- R_prom_ev - R_prom_con
          
          ## Optimalidad
          
          ### Simplex reticular
          
          Xm_SR <- generar_combinaciones(dde_SR$Proporciones, m)
          
          opt_SR <- optimalidad(Xm_SR, "_SR_Prop")
          
          ### Vértices extremos
          
          Xm_EV_prop <- try(generar_combinaciones(dde_ev$Proporciones, m))
          
          if(is.data.frame(Xm_EV_prop)){
            
            opt_EV_Prop <- optimalidad(Xm_EV_prop, "_EV_Prop")
            
            Xm_EV_Pseudo <- generar_combinaciones(dde_ev$Pseudocomponentes, m)
            
            opt_EV_Pse <- optimalidad(Xm_EV_Pseudo, "_EV_Pseudo")
            
          } else{
            
            opt_EV_Prop <- data.frame(A_opt = NA, D_opt = NA, G_opt  = NA, V_opt  = NA, 
                                      H_max = NA,
                                      A_eff = NA, D_eff = NA, G_eff = NA) %>% 
              dplyr::rename_with(~ paste0(.x, "_EV_Prop"))
            
            opt_EV_Pse <- data.frame(A_opt = NA, D_opt = NA, G_opt  = NA, V_opt  = NA, 
                                     H_max = NA,
                                     A_eff = NA, D_eff = NA, G_eff = NA) %>% 
              dplyr::rename_with(~ paste0(.x, "_EV_Pseudo"))
            
          }
          
          ### CONVEXSIM 
          
          Xm_CXS_Prop <- generar_combinaciones(dde_conv$Proporciones, m)
          
          opt_CXS_Prop <- optimalidad(Xm_CXS_Prop, "_CXS_Prop")
          
          Xm_CXS_Pseudo <- generar_combinaciones(dde_conv$Pseudocomponentes, m)
          
          opt_CXS_Pse <- optimalidad(Xm_CXS_Pseudo, "_CXS_Pseudo")
          
          
          data.frame(df[x, ], p,
                     n_conv, n_ev, 
                     tn_SR, tn_EV, 
                     dE_SR, dE_EV, 
                     R_prom_ev, R_prom_con, R, 
                     opt_SR, 
                     opt_EV_Prop, opt_EV_Pse, 
                     opt_CXS_Prop, opt_CXS_Pse)
          
        } else{
          
          data.frame(df[x, ], p,
                     n_conv = NA, n_ev = NA, 
                     tn_SR = NA, tn_EV = NA, 
                     dE_SR = NA, dE_EV = NA, 
                     R_prom_ev = NA, R_prom_con = NA, R = NA, 
                     opt_SR = NA, 
                     opt_EV_Prop = NA, opt_EV_Pse = NA, 
                     opt_CXS_Prop = NA, opt_CXS_Pse = NA)
          
        }
        
      })
    
    # Se van a ir guardando uno a la vez, de forma preliminar
    # en caso de algún código llegue a fallar. 
    
    save(preliminar,
         file = paste0("resultados/resultados_m3_",
                       preliminar$p[1],
                       ".RData"))
    
  })


