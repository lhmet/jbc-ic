
time_step <- function(dates, verbose = TRUE) 
{
 # dates <- x[["dates"]]
  intervals <- dates %>%
    diff() 

  intervals_freq <- intervals %>%
    table()
  
  deltat <- intervals_freq %>%
    which.max() %>%
    names() %>%
    as.numeric()
  
  ## time unit
  time_unit_str <- intervals %>%
    min() %>%
    capture.output() %>%
    strsplit(" ") %>%
    unlist()
  
  time_step_str <- paste(deltat, 
                        time_unit_str[length(time_unit_str)],
                        collapse = " ")
  if (verbose) {
    cat("Dataset time step:", time_step_str, "\n")
    tab_interv <- intervals_freq %>%
      prop.table() %>%
      magrittr::multiply_by(100) %>%
      round(2)
    #tab <- round(prop.table(table(diff(X$date)))*100,2)
    intervals_df <- tibble::data_frame(time = names(tab_interv),
                                       freq = paste(tab_interv, "%"))
    print(intervals_df)
  }
  return(time_step_str)
}



time_span <- function(dates, full_day = FALSE, tz = "UTC"){
  # dates <- pga$date
  tz(dates) <- "UTC"
  date_min <- min(dates)
  date_max <- max(dates)
  
  if (!full_day) {
   return(tibble::data_frame(date_min, date_max))  
  }
  
  ## obtain start and end date
  m.min <- dates %>%
    lubridate::minute() %>%
    min() %>%
    as.character() %>%
    as.integer()
  
  m.min <- dates %>%
    lubridate::minute() %>%
    max() %>%
    as.character() %>%
    as.integer()
  
  h.min   <- dates %>%
    lubridate::hour() %>%
    min() %>%
    as.character() %>%
    as.integer()
  
  h.max   <- dates %>%
    lubridate::hour() %>%
    max() %>%
    as.character() %>%
    as.integer()
  
  sdate <- as.POSIXct(paste(as.Date(date_min)," ", 
                            h.min,":",m.min, ":00",
                            #0,":",30, ":00",
                            sep = ""), 
                      tz = tz)
  edate <- as.POSIXct(paste(as.Date(date_max)," ",
                            h.max,":",m.min, ":00",
                            #0,":",0, ":00",
                            sep = ""), 
                      tz = tz)
  return(tibble::data_frame(sdate, edate))
}

#time_span(pga$date)
#time_step(pgaa$date)



# Funçao para completar as datas ausentes no conjuno de dados
complete_dates <- function(x, 
                           group = "site",
                           full_day = TRUE,
                           verbose = FALSE,
                           tz = "UTC"){
  # x <- tar_data_sel; 
  if (is.null(group)) {
    x$site <- "my_site"
    group <- "site"
    group_was_null <- TRUE
  }
  xDT <- data.table::data.table(x, key = c("date", group))
  rm(x)
  if (anyDuplicated(xDT) > 0) {
    message(dup_rows, 
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
  date_range <- time_span(dates = x[["date"]], full_day, tz)
  
  # dates vector with constant and regular time step
  all_dates <- seq(date_range[[1]], 
                   date_range[[2]], 
                   by = time_step(x[["date"]], verbose))
  
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
  
  if (group_was_null) {
    xc$site <- NULL
    rm(group)
  }
  return(as_tibble(xc))
}


