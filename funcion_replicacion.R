# replicación de diseños --------------------------------------------------


#' Replicación de diseños
#'
#' @param df diseño sin réplicas en formato data.frame
#' @param n cantidad de réplicas
#'
#' @return el diseño replicado n veces

replicar <- function(df, n) {
  
  dplyr::bind_rows(replicate(n, df, simplify = FALSE))
  
}
