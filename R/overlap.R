convert_unit <- function(x, unit = "year"){
  mult <- c(second = 1, minute = 60, hour = 3600, mday = 86400, 
            wday = 86400, yday = 86400, day = 86400, week = 604800, 
            month = 60 * 60 * 24 * 365/12, year = 60 * 60 * 24 * 
              365)
  as.numeric(x)/unname(mult[unit])
}


overlap <- function(data, 
                    site1, 
                    site2,
                    time_unit = "day"){
  
  # data <- dados_spr; site1 = "A801"; site2 = "A818"
  # data <- dados_check; site1 = "A801"; site2 = "A818"
  
  # dados meteo com id de intervalo, formato wide, cada ema uma coluna 
  # data <- data_int_wide; site1 = "A801"; site2 = "A829"; time_unit = "day"
  
  stopifnot(site1 %in% data$target | site2 %in% data$target)
    
  # passo de tempo
  dt <- diff(data$date)
  u <- units(dt)
  
  tb_freq <- table(sort(dt))
  dt <- as.numeric(names(tb_freq[which.max(tb_freq)]))
  
  if(ncol(data) < 4){ # site, date, var.name
    var.name <- names(data)[ncol(data)]
    x <- data %>%
      filter(site %in% c(site1, site2)) %>%
      spread_("site", var.name)
    #nrow(x)
    rec_overlap <- nrow(x[complete.cases(x), ])
    # conversão de unidades
    out <- convert_unit(lubridate::dhours(rec_overlap))
    return(rec_overlap)
  }
  # aws data in columns
  data2sites <- dplyr::select(data, one_of(c("date", site1, site2)))
  rec_overlap <- nrow(data2sites[complete.cases(data2sites), ])
  # conversão de unidades
  out <- convert_unit(lubridate::ddays(rec_overlap), unit = time_unit)
  #rec_overlap_u <- lubridate::duration(rec_overlap, units = u)
  #rec_overlap_u <- convert_unit(rec_overlap, unit = time_unit)
  return(out)
}


overlap_combs <- function(data, sites_comb) {
  # data = data_int_wide
  # sites_comb = target_combs_int
  map_dbl(1:nrow(sites_comb), function(ir) {
    overlap(data,
      site1 = sites_comb$aws1[ir],
      site2 = sites_comb$aws2[ir],
      time_unit = "day"
    )
  })
}

#overlap_combs(data_int_wide, target_combs_int)