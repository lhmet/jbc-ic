source("../R/network-dists.R")
#source("../R/station2staions-dists.R")

coords_target <- readRDS("../output/tar-info-inmet-2008-2016-4yrs-south.rds") %>%
  filter(name == "Santa Maria")

coords_ngb <- readRDS("../output/tar-info-inmet-2008-2016-4yrs-south.rds") %>%
  filter(name != "Santa Maria")

coords_ngb_o <- arrange(coords_ngb, site)

dists_stns <- network_dists(netw_ref = coords_ngb,
                            netw_aux = coords_ngb, 
                            lonlat = TRUE, 
                            allpairs = TRUE
                            )
# add nome da estação para conferir dists
dists_stns <- select(coords_ngb, site, name, alt) %>%
  full_join(., dists_stns, by = c("site" = "aux")) %>%
  arrange(ref) %>%
  group_by(ref) %>%
  arrange(ref, dis) %>%
  select(ref, site, name, dis, alt) %>%
  ungroup()

dists_stns
