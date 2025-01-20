
# generar diseño de Vértices extremos -------------------------------------


#' Función generadora de diseños de vértices extremos
#'
#' @param p cantidad de componentes
#' @param m grado del retículo
#' @param ax si el diseño es con axiales o no (TRUE or FALSE)
#' @param ls límite superior del diseño
#' @param n cantidad de réplicas
#'
#' @return diseño de vértices extremos
#' @export 
#'
#' @examples
generar_diseno_EV <- function(p, m, ax = TRUE, li, ls, n){
  
  source("funcion_replicacion.R")
  source("funcion_axiales.R")
  
  m <- m - 1
  
  li <- as.numeric(li)
  ls <- as.numeric(ls)
  
  # Diseño empleado para axiales
  
  dde_ax <- mixexp::Xvert(nfac = p, 
                          uc = ls, 
                          lc = li, 
                          ndm = 0, # 0 --> m = 1
                          plot = FALSE,
                          pseudo = FALSE) %>% 
    signif(3)
  
  
  axiales <- crear_axiales(dde_ax)
  
  centroide <- mixexp::Xvert(nfac = p, 
                             uc = ls, 
                             lc = li, 
                             ndm = m, # 0 --> m = 1
                             plot = FALSE,
                             pseudo = FALSE) %>% 
    signif(3) %>% 
    dplyr::filter(dimen == p - 1) %>% 
    dplyr::select(-dimen)
  
  dde <- mixexp::Xvert(nfac = p, 
                       uc = ls, 
                       lc = li, 
                       ndm = m, # 0 --> m = 1
                       plot = FALSE,
                       pseudo = FALSE) %>% 
    signif(4) %>%
    dplyr::select(-dimen)
  
  if(isTRUE(ax)){
    
    dde <- rbind.data.frame(dde,
                            axiales) 
    
  } else{
    
    #do nothing
    
  }
  
  dde <- replicar(dde, n) 
  
  dde_pseudo <- 1:nrow(dde) %>% 
    purrr::map_df(function(x){
      
      (dde[x,] - round(li, 4))/(1-sum(round(li, 4)))
      
    }) %>% 
    signif(3)
  
  list("Proporciones" = dde, 
       "Pseudocomponentes" = dde_pseudo, 
       "Centroide" = centroide)
  
}
