##arrange df vars by position
##'vars' must be a named vector, e.g. c("var.name"=1)
arrange_vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}




replace_inf <- function(x) ifelse(!is.finite(x), NA, x)
#replace_inf(c(Inf, -Inf, 1, NA))

# Apply fun dealing with vectors values that may be all NA ---------------------
fun_NA <- function(x, fun, ...){
  if(all(is.na(x))) return(NA)
  fun(x, ...)
}


n_valid <- function(x) sum(!is.na(x))

percent_valid <- function(x, N = NULL) {
  if (is.null(N)) return(n_valid(x)/length(x) * 100)
  
  n_valid(x)/N * 100
}

check_firstNA <- function(x) !is.na(first(x))

replace_NA <- function(x, fill.value = 0) {
  #x <- summary_qc_j$tot
  replace(x, is.na(x), fill.value)
}


point <- scales::format_format(big.mark = ".",
                               decimal.mark = ",",
                               scientific = FALSE)
