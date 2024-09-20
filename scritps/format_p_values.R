format_p_values <- function(p_values) {
  formatted_values <- sapply(p_values, function(p) {
    if (is.na(p)) {
      return(NA)  # Retorna NA se o valor p for NA
    } else if (p < 0.001) {
      return("p<0.001")
    } else if (p < 0.05) {
      return(sprintf("p<0.05"))
    } else {
      return(sprintf("p=%.2f", p))
    }
  })
  
  return(formatted_values)
}