code_target <- function(x){
  # x <- by_target_int[["data"]][[1]]
  filter(x, is_target)[["site"]][1]
}

stations_neighbors2 <- function(aws_data, neighbors,  M = c(10, Inf), keep.target = TRUE) {
  stopifnot(all(c("ref", "aux") %in% names(neighbors)), sign(M) == 1)
  # aws_data = by_target_int[["data"]][[1]]; M = 10; neighbors = neigh; keep.target = TRUE
  aws_id <- code_target(aws_data)
  neigh_codes <- dplyr::filter(neighbors, ref == aws_id) %>%
    dplyr::filter(proximity <= M + 1) %>%
    dplyr::pull(aux)
  
  if (!keep.target) {
    neigh_codes_strict <- neigh_codes[!neigh_codes %in% aws_id]
    return(neigh_codes_strict)
  }
  neigh_codes
}



stations_neighbors <- function(neighbors, aws_id, M = c(10, Inf), keep.target = TRUE) {
  stopifnot(all(c("ref", "aux") %in% names(neighbors)), sign(M) == 1)
  # aws_id = "A899"; M = Inf
  neigh_codes <- dplyr::filter(neighbors, ref == aws_id) %>%
    dplyr::filter(proximity <= M + 1) %>%
    dplyr::pull(aux)

  if (!keep.target) {
    neigh_codes_strict <- neigh_codes[!neigh_codes %in% aws_id]
    return(neigh_codes_strict)
  }
  neigh_codes
}


# Select data for Spatial Regression Test
select_data_SRT <- function(
                            data,
                            neighbors,
                            aws_code,
                            m,
                            obs_unit = c("date", "int")) {
  #TEST DATA
  # data = ungroup(interval_data); neighbors = neigh; aws_code = "A882"; m = 10;obs_unit = c("date", "int")
  neigh_codes <- stations_neighbors(
    neighbors,
    aws_id = aws_code,
    M = m
  )

  
  data4srt <- dplyr::filter(
    data, 
    site %in% neigh_codes) %>%
    dplyr::mutate(target = aws_code)#, 
    #              is_target = if_else(site == aws_code, TRUE, FALSE)
    #              )
    

  return(data4srt)
}
