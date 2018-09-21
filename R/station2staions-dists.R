#' Distances between two meteorological networks
#' 
#' @importFrom dplyr %>%
#' 
#' @param netw_ref data.frame with columns site, lon, lat
#' @param netw_aux data.frame with columns site, lon, lat
#' @param dx_max optional, threshold distance (in km) below which stations are keep.  
#'
#' @return
#' @export
#'
#' @examples
station2stations_dists <- function(netw_ref = coords_target, 
                          netw_aux = coords_ngb,
                          dx_max = NA,
                          ...){
  
  pts1 <- dplyr::select(netw_ref, lon, lat) %>%
    as.data.frame(); # head(pts2)
  pts2 <- dplyr::select(netw_aux, lon, lat) %>%
    as.data.frame()
  # diatance matrix
  dists <- raster::pointDistance(p1 = pts1, 
                                 p2 = pts2, 
                                 #lonlat = TRUE
                                 ...
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
  
  dists <- rename(dists, "distance_km" = .) %>%
    mutate(site = netw_aux %>%
             select(site) %>% 
             dplyr::pull() %>%
             as.character()
           ) %>%
    arrange(distance_km)
  
    return(dists)
}