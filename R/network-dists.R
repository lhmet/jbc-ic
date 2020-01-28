#' Distances between two meteorological networks
#' 
#' @importFrom dplyr %>%
#' 
#' @param netw_ref data.frame with columns site, lon, lat
#' @param netw_aux data.frame with columns site, lon, lat
#' @param dx_max optional, threshold distance (in km) below which stations are keep.
#'
#' @details '...' include paramaters like allpairs, useful when we want
#' to consider all possible combinations between station. 'lonlat', logical to
#' coordinates in lat e lon. dx_max to return only stations within a specified distance.
#' @return
#' @export
#'
#' @examples
network_dists <- function(netw_ref, 
                          netw_aux = netw_ref,
                          all_pairs = TRUE,
                          lon_lat = TRUE,
                          dx_max = NA
                          ){
  
  # netw_ref = ; netw_aux = 
  
  
  pts1 <- dplyr::select(netw_ref, lon, lat) %>%
    as.data.frame() # head(pts2)
  pts2 <- dplyr::select(netw_aux, lon, lat) %>%
    as.data.frame()
  # diatance matrix
  dists <- raster::pointDistance(p1 = pts1, 
                                 p2 = pts2, 
                                 allpairs = all_pairs,
                                 lonlat = lon_lat
                                 ) %>%
    as.data.frame() %>% 
    # to km
    magrittr::divide_by(1000) 
  
  # ckeck
  #stopifnot(ncol(dists) == nrow(netw_aux), 
  #          nrow(dists) == nrow(netw_ref))
  
  if (!is.na(dx_max)) dists[dists > dx_max] <- NA
  
  # ref stations code/id
  ref_sites <- netw_ref %>% 
    dplyr::select(site) %>% 
    dplyr::pull() %>%
    as.character()
  
  colnames(dists) <- netw_aux %>%
    select(site) %>% 
    dplyr::pull() %>%
    as.character()
  
  dists <- mutate(dists, ref = ref_sites) %>%
    tibble::as_tibble() %>%
    arrange_vars(., c("ref" = 1)) 
  #dists
  
  ref_aux_dis <- dists %>%
    tidyr::gather(aux, dis, -ref) %>% 
    dplyr::filter(!is.na(dis)) %>%
    dplyr::arrange(ref, dis)
  
  return(ref_aux_dis)
}