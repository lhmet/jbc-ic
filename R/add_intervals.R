
#' Adiciona identificador do intervalo
#'
#' @param dlydata tibble com dados diários 
#' @param interval_size numeric, tamanho do intervalo. Default é 60 dias.
#'
#' @return
#' @export
#'
#' @examples
#'  dly_data <- daily_data %>%
#'   group_by(site) %>%
#'   slice(1:(sample(c(3288, 3000, 2000, 1000, 900, 700), size = 1))) %>%
#'   ungroup()
#'   add_intervals(dlydata = dly_data, interval_size = 60)
add_intervals <- function(dlydata, interval_size = 60){
  
  stopifnot("site" %in% names(dlydata))
  
  data_interval <- dlydata %>%
    group_by(site) %>%
    mutate(
      # Número de dias de cada EMA
      ndays = n(),
      # identificador do intervalo de 60 dias (1 = 1° intervalo, 2 = 2° intervalo, ...)
      int = rep(1:(trunc(ndays[1]/interval_size) + 1), 
                each = interval_size)[1:(ndays[1])],
      ndays = NULL
    ) %>%
    arrange_vars(c("int" = 3))
  return(data_interval)
}