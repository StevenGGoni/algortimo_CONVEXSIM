# creación de axiales -----------------------------------------------------

#' Creador de puntos axiales
#'
#' @param df diseño sin réplicas en formato data.frame con m = 1 --> ndm = 0
#'
#' @return los puntos axiales requeridos

crear_axiales <- function(df){
  
  centro <- df[nrow(df), -ncol(df)]
  
  purrr::map2_dfr(df[-nrow(df), -ncol(df)], centro,
                  ~ (.x+.y)/2)
  
}
