
#' Generar combinaciones para optimalidad
#'
#' @param data objeto data.frame con el diseño del experimento
#'
#' @returns objeto data.frame con el diseño del experimento y sus combinaciones
#' @export
#'
#' @examples
generar_combinaciones <- function(data, m) {
  
  # Obtener nombres de las columnas
  nombres <- colnames(data)
  
  # Generar combinaciones de 2 y 3 columnas
  
  if(m == 1){
    
    data_combinado <- data
    
  } else{
    
    if(m == 2){
      
      combinaciones <- c(
        combn(nombres, 2, simplify = FALSE))
      
      # Calcular los productos para cada combinación
      resultados <- purrr::map(combinaciones, ~ {
        producto <- purrr::reduce(data[.x], `*`)
        colname <- paste(.x, collapse = "*")
        setNames(data.frame(producto), colname)
      })
      
      # Combinar los resultados en un solo data.frame
      combinaciones_resultado <- dplyr::bind_cols(resultados)
      
      # Agregar combinaciones al data.frame original
      data_combinado <- dplyr::bind_cols(data, combinaciones_resultado)
      
    } else{
      
      combinaciones <- c(
        combn(nombres, 2, simplify = FALSE), # Combinaciones de 2 variables
        combn(nombres, 3, simplify = FALSE))  # Combinaciones de 3 variables
      
      # Calcular los productos para cada combinación
      resultados <- purrr::map(combinaciones, ~ {
        producto <- purrr::reduce(data[.x], `*`)
        colname <- paste(.x, collapse = "*")
        setNames(data.frame(producto), colname)
      })
      
      # Combinar los resultados en un solo data.frame
      combinaciones_resultado <- dplyr::bind_cols(resultados)
      
      # Agregar combinaciones al data.frame original
      data_combinado <- dplyr::bind_cols(data, combinaciones_resultado)
      
      
      
    }
    
  }
  
  return(data_combinado)
  
}