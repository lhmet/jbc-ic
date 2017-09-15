#' Detect or identify consecutive repeating values in a vector
#'
#' @param x numeric vector
#' @param thresh threshold value above which events of consecutive repeating values will be defined 
#' @param min.steps the minimum number of time steps a value should repeat to be a event. Any events whose duration is shorter than this will be skipped.
#' @param numerate logical,if set to be true events are numbered  
#' 
#' @return returns a logical \code{vector} of length \code{length(x)}. If 
#' \code{numerate = TRUE} returns a numeric \code{vector}. Periods between
#'  events are left as NA.   
#'  
#' @export
#'
#' @examples
#' v <- c(0, 0, 0, 0.2, 0.2, 0, NA, NA, NA, NA, 0.5, 0.5, 0, 0, 0, 10, 0, 3, 3)
#' dates <- seq(as.POSIXct("2000-01-01 12:00:00"),
#'              as.POSIXct("2000-01-01 12:00:00") + 3600*(length(v)-1),
#'              by = "hour")
#' d <- data.frame(date = dates, prec = v)
#' d
#' data.frame(d,
#'       is_crv2 = crv_detect(x, thresh = 0, min.steps = 2),
#'       id_crv2 = crv_detect(x, thresh = 0, min.steps = 2, numerate = TRUE),
#'       is_crv3 = crv_detect(x, thresh = NA, min.steps = 3),
#'       id_crv3 = crv_detect(x, thresh = NA, min.steps = 3, numerate = TRUE))
#'
crv_detect <- function(x, thresh = 0, min.steps = 2, numerate = FALSE){
  
  # x <- temp; thresh = 0; min.steps = 2; numerate = TRUE
  
  default <- -Inf
  # to deal with crv of NAs and 0s 
  if(is.na(thresh)) {
    default <- -9999
    thresh <- -Inf
  } 
  
  # because in rle() each NA is treated as a run of length 1
  x <- replace(x, which(is.na(x)), values = default)
  runs <- rle(x)
  is_crv <-  with(runs, values > thresh & lengths >= min.steps)
  # index of lengths
  idx <- rep(seq_len(length(runs$values)), runs$lengths)
  
  # expand back into time series
  crv_event_ts <- is_crv[idx]
   if(!numerate) return(crv_event_ts)
  
   # numerate events of crv
   crv_enum <- cumsum(is_crv) * is_crv
   crv_enum <- ifelse(crv_enum == 0, NA, crv_enum)
   crv_enum_ts <- crv_enum[idx]
  
  return(crv_enum_ts)
}


#' Events summary of consecutive repeating values
#'
#' @param x a data frame with columns site, id and 
#' @param var.name 
#'
#' @return An object of the same class as .data. One grouping level will be dropped.
#' @export
#'
#' @examples
crv_summary <- function(.data, var.name = "prec"){
 # x <- structure(list(date = structure(c(946728000, 946731600, 946735200, 
 #  946738800, 946742400, 946746000, 946749600, 946753200, 946756800, 
 #  946760400, 946764000, 946767600, 946771200, 946774800, 946778400, 
 #  946782000, 946785600, 946789200, 946792800), class = c("POSIXct", 
 #                                                         "POSIXt"), tzone = ""), prec = c(0, 0, 0, 0.2, 0.2, 0, NA, NA, 
 #                                                                                          NA, NA, 0.5, 0.5, 0, 0, 0, 10, 0, 3, 3), site = c("A900", "A900", 
 #                                                                                                                                            "A900", "A900", "A900", "A900", "A900", "A900", "A900", "A900", 
 #                                                                                                                                            "A900", "A900", "A900", "A900", "A900", "A900", "A900", "A900", 
 #                                                                                                                                            "A900")), .Names = c("date", "prec", "site"), row.names = c(NA, 
 #                                                                                                                                                                                                        -19L), class = "data.frame")
  #.data = exdata; var.name = "tar"
  stopifnot(is.data.frame(.data), "site" %in% names(.data))
  
  dtfm <- data.frame(.data, 
                     id = crv_detect(.data[[var.name]], 
                                     numerate = TRUE))
  
  dtfm <- dplyr::rename_(dtfm, value = var.name)
  dtfm <- dplyr::filter(dtfm, !is.na(id)) 
  dtfm$aws <- dtfm$site
  by_id <- dplyr::group_by(dtfm, site, id)
  s <- dplyr::summarise(by_id, 
                 #aws = first(aws),
                 begin = first(date),
                 end = last(date),
                 value = unique(value))
  ungroup(s)
}