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
network_dists <- function(netw_ref = tar_info, 
                          netw_aux = meta_emc,
                          dx_max = NA,
                          ...){
  
  pts1 <- dplyr::select(tar_info, lon, lat) %>%
    as.data.frame(); # head(pts2)
  pts2 <- dplyr::select(meta_emc, lon, lat) %>%
    as.data.frame()
  # diatance matrix
  dists <- raster::pointDistance(p1 = pts1, 
                                 p2 = pts2, 
                                 ...) %>%
    as.data.frame() %>% 
    # to km
    magrittr::divide_by(1000) 
  
  # ckeck
  stopifnot(ncol(dists) == nrow(netw_aux), 
            nrow(dists) == nrow(netw_ref))
  
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
    tibble::as_data_frame() %>%
    arrange_vars(., c("ref" = 1)) 
  #dists
  
  ref_aux_dis <- dists %>%
    tidyr::gather(aux, dis, -ref) %>% 
    dplyr::filter(!is.na(dis)) %>%
    dplyr::arrange(ref, dis)
  
  return(ref_aux_dis)
}