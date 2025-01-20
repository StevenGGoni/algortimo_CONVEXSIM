#' Función para obtener límites superiores e inferiores acomodados
#'
#' @param lista objeto lista resultante de CONVEXSIM
#'
#' @return data.frame con los límites
#' @export
#'
#' @examples
obtener_limites <- function(lista){
  
  lista %>% 
    purrr::map_dfr(\(x) unlist(x))
  
}

