expected_rows <- function(x){
  tibble::data_frame(nr = nrow(x), 
             start = min(x$date), 
             end = max(x$date),
             nr_exp = length(seq(start, end, by = "hour"))*length(sort(unique(x$site))))  
}