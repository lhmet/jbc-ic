# Script to generate ../output/aws-neighbors-south-br.RDS
# easypackages::libraries(c("dplyr", "tidyverse", "lubridate"))
# source("../R/network-dists.R")

get_aws_neighborhood <- function(...,
                                 outfile = "../output/aws-neighbors-south-br.RDS") {
  stopifnot(file.exists(outfile))

  dists <-
    network_dists(...,
      # netw_ref = var_info,
      # netw_aux = var_info,
      dx_max = NA,
      lon_lat = TRUE
    ) %>%
    dplyr::arrange(ref, dis) %>%
    dplyr::group_by(ref) %>%
    # incluindo proximity index (1: closer)
    dplyr::mutate(proximity = dplyr::dense_rank(dis)) %>%
    dplyr::ungroup()
  if (!is.null(outfile)) {
    base::saveRDS(dists, file = outfile)
  }

  return(dists)
}