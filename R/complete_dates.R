# Funçao para completar as datas ausentes no conjuno de dados
complete_dates <- function(x, group = "site", time_step = "hours"){
  # x <- awsd
  xDT <- data.table::data.table(x, key = c("date", group))
  rm(x)
  if(anyDuplicated(xDT) > 0){
    message( 
            " duplicated dates:", 
            " keeping the first data record.")
  }
  # data table with duplicated rows removed
  #key(xDT)
  xDT <- unique(xDT)
  x <- as.data.frame(xDT); rm(xDT)
  
  # number of distinct values in the group variable
  group_var <- dplyr::distinct_(x, group) %>%
    dplyr::select_(group) %>%
    t() %>% c()
  
  # time span
  time_span <- range(x$date)
  # dates vector with constant and regular time step
  all_dates <- seq(time_span[1], 
                   time_span[2], 
                   by = time_step)
  
  # length(all_dates) * length(group_var)
  all_dates <- expand.grid(date = all_dates, 
                           site = group_var) 
  all_dates <- data.frame(all_dates) %>%
    dplyr::mutate(site = as.character(site))
  # nrow(all_dates)
  
  
  # join time series to match all_dates lenght  
  xc <- x %>%
    dtplyr::tbl_dt() %>%
    dplyr::right_join(dtplyr::tbl_dt(all_dates), 
                      by = c("date", group))
  
  rm(x, all_dates)
  
  return(as_tibble(xc))
  
}


