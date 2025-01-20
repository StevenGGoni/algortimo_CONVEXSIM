#' Función de optomalidad y eficiencia
#'
#' @param X data.frame
#' @param tipo tipo de región Original - Convexsim - EV (prop o pseudo)
#'
#' @returns criterios de optimalidad y eficiencia
#' @export
#'
#' @examples
optimalidad <- function(X, tipo){
  
  q <- ncol(X)
  
  n <- nrow(X)
  
  X <- as.matrix(X)
  
  inv_t_XX <- tryCatch(expr = inv(t(X) %*% X),
                       error = function(e) NA)
  
  #### A -  Optimalidad
  
  A_opt <- tryCatch(expr = tr(inv_t_XX), 
                    error = function(e) NA)
  
  #### A - Eficiencia
  
  A_eff <- q/(n*A_opt)
  
  #### D - Optimalidad
  
  D_opt <- tryCatch(expr = det(inv_t_XX), 
                    error = function(e) NA)
  
  #### D - Eficiencia
  
  D_eff <- (det(t(X) %*% X)^(1/q))/n
  
  #### Leverage
  
  H <- tryCatch(expr = diag(X %*% inv_t_XX %*% t(X)),
                error = function(e) NA)
  
  H_max <- max(H)
  
  #### G - Optimalidad
  
  G_opt <- mean(H)/max(H)
  
  #### G - Eficiencia
  
  G_eff <- q/(n*G_opt)
  
  #### V - Optimalidad
  
  V_opt <- mean(H)
  
  data.frame(A_opt, D_opt, G_opt, V_opt, 
             H_max,
             A_eff, D_eff, G_eff) %>% 
    dplyr::rename_with(~ paste0(.x, tipo))
  
}
