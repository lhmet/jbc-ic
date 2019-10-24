
#globalVariables(names = c("site", "date"))

daily_summary <- function(hly_data, functions = c("mean"), col_vars = "tavg"){
  
  # TEST DATA
  # hly_data <- rio::import("output/s08_data_sel.rds") %>% as_tibble
  obs_units <- c("site", "date")
  test_obs_unit <-  obs_units %in% names(hly_data)
  stopifnot(all(test_obs_unit))
  
  if(is.null(col_vars) | missing(col_vars)){
    # all vars, except site and date
    col_vars <- names(hly_data)[!(names(hly_data) %in% obs_units)]  
  }
  
  # As all meteo vars should be continuous
  # hly_data %>% summarise_each(funs = is_bare_double) %>% flatten_lgl()
  # microbenchmark::microbenchmark(  
  dly_summary <- hly_data %>%
    group_by(site, date = as.Date(date)) %>%
    summarise_at(col_vars, .funs = functions, na.rm = TRUE) %>%
    ungroup() %>%
    mutate_at(col_vars, .funs = ~if_else(is.nan(.), NA_real_, .))
  # , times = 1)    
  return(dly_summary)
}

