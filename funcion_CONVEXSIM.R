
# CONVEXSIM ---------------------------------------------------------------

#' Algortimo CONVEXSIM (I o S)
#'
#' @param ls límites superiores (Formato df)
#' @param tipo I o S, para determinar que tipo de CONVEXSIM se usará
#'             por defecto se usa I 
#'
#' @return nuevos límites inferiores y superiores con forma símplex
#' @export
#'
#' @examples
convexsim <- function(ls, tipo = "I"){
  
  # El paso i y ii son ejecutados fuera de esta función
  # ya que solo se recibirán límites superiores, asumiendo que los
  # inferiores son 0 y alcanzables
  # Además, solo se reciben diseños que sean de VE
  
  # Paso iii. Determinar el valor mínimo de b_g ----
  
  # ls <- test
  ls_f <- ls
  
  p <- ncol(ls) # Cantidad de componentes
  
  b_g <- min(ls) # valor de b_g
  
  g <- which(ls == b_g)[1] # posición en el vector de b_g
  
  a_i <- rep((1 - b_g)/(p - 1),p)
  
  a_i[g] <- 0 # en g se mantiene igual
  
  trazabilidad <- "Región regular" # como fue generado convexsim
  
  # Si a_i >= b_i, el diseño no tiene sentido, por tanto se ajusta
  
  comprobar <- any(as.double(a_i)>=as.double(as.numeric(ls)))
  
  
  if(comprobar){
    
    trazabilidad <- "Región especial" # como fue generado convexsim
    
    a_i <- ls/2
    
    a_i[g] <- 0 # en g se mantiene igual
    
  } 
  
  li <- as.numeric(a_i)
  
  # Paso iv. Calculo de a_g ----
  
  R_min <- min(ls[-g] - li[-g])
  
  R_a <- 1 - sum(li)
  
  a_g <- R_a - R_min
  
  if(any(a_g < 0, 
         isTRUE(all.equal(as.numeric(a_g), as.numeric(b_g))),
         tipo == "S")){ 
    # caso especial de convexsim-I | convexsim - S
    # O bien cuando a_g no se puede obtener normalmente
    
    if(isTRUE(all.equal(as.numeric(a_g), 
                        as.numeric(b_g)))){
      
      a_i <- a_i + (R_min/(p - 1)) # se refiere a a_i*
      
      li <- a_i
    }
    
    a_g <- 0
    
  }
  
  li[g] <- a_g # nuevos límites inferiores
  
  # paso v. Ajuste de las restricciones superiores ----
  
  R_a <- 1 - sum(li)
  
  ls <- li + R_a
  
  # paso vi. Validación con transformación en pseudocomponentes ----
  
  if(any(round(as.double(as.numeric(ls)),4) > 
         round(as.double(as.numeric(ls_f)),4))){
    
    CS <- "S"
    
  } 
  
  else{
    
    CS <- "I"
    
  }
  
  if(sum(li)>=1){
    
    validador <- "Incorrecto" }
  
  else{
    
    if(all(round((li-li)/(1-sum(li)),1) == 0) &
       all(round((ls-li)/(1-sum(li)),1) == 1)){
      
      validador <-"Correcto"
      
    } else{
      
      validador <- "Incorrecto"
    }}
  
  # Return ----
  
  list("Límite inferior" = as.numeric(li),
       "Límite superior" = as.numeric(ls),
       "Diseño" = validador,
       "Trazabilidad" = trazabilidad, 
       "CS" = CS)
  
}

