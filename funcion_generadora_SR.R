
# generar diseño simplex reticular ----------------------------------------

#' Función generadora de diseños símplex reticular
#'
#' @param p cantidad de componentes
#' @param m grado del retículo
#' @param ax si el diseño es con axiales o no (TRUE or FALSE)
#' @param n cantidad de réplicas
#'
#' @return diseño símplex reticular
#' @export
#'
#' @examples
generar_diseno_SR <- function(p, m, ax = TRUE, n){
  
  source("funcion_replicacion.R")
  source("funcion_axiales.R")
  
  variables <- stringr::str_c("x", 1:p)
  
  centroide <- data.frame(t(rep(1/p, p))) %>% 
    setNames(variables)
  
  dde_ax <- rbind.data.frame(mixexp::SLD(p, 1),
                             centroide) %>% 
    dplyr::mutate(dummy = 0)
  
  axiales <- crear_axiales(dde_ax)
  
  dde <- mixexp::SLD(p, m)  %>% 
    signif(3)
  
  if(isTRUE(ax)){
    
    if(m != 3){
      
      dde <- rbind.data.frame(dde,
                              centroide, 
                              axiales) 
      
      
    }else{
      
      dde <- rbind.data.frame(dde,
                              axiales) 
      
    }
    
  } else{
    
    if(m != 3){
      
      dde <- rbind.data.frame(dde,
                              centroide) 
      
    }else{
      
      dde <- rbind.data.frame(dde) 
      
    }
    
  }
  
  dde <- replicar(dde, n)
  
  list("Proporciones" = dde, 
       "Centroide" =  centroide)
  
}
