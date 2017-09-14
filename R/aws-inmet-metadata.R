# Script para obter metadados do INMET

# a partir da combinação das informações disponíveis no website (via inmetdown)
# http://www.inmet.gov.br/portal/index.php?r=estacoes/estacoesAutomaticas
# e nas informações extraídas dos arquivos excel processados (via inmetrold)
# uma vez que as informações das EMAs desativadas não são disponibilizadas
# no website

# Info obtidas via website  ----------------------------------------------------
#format(Sys.time(), '%d %B, %Y')
#[1] "11 setembro, 2017"
easypackages::libraries(c("inmetdown", "tidyverse", "lubridate"))
# dados obtidos do site do inmet
stns <- aws_stations() %>%
  select(-url) %>%
  rename("site" = id, 
         "name" = city)
stns
stns_sul <- filter(stns, state %in% c("RS", "SC", "PR"))
stns_sul %>% dplyr::arrange(desc(start)) %>% data.frame()

saveRDS(stns_sul, file = "~/Dropbox/github/my_reps/lhmet/jbc-ic/data/info-inmet-south-from-website.rds")

# incluindo EMAs desativadas  -------------------------------------------------
info <- readRDS("/home/hidrometeorologista/Dropbox/github/my_reps/lhmet/inmetr_old/output/sul/joined/info-inmet-sul-20000922-20161231.rds")
info
# Estação desativada?
desativadas <- filter(info, !site %in% stns_sul$site)
desativadas
# substituição das coordenadas por valores mais precisos 
# http://www.unidata.ucar.edu/mailing_lists/archives/idd-brasil/2009/msg00075.html
# http://www.dca.ufcg.edu.br/posgrad_met/dissertacoes/WagnerdeAragaoBezerra_2013.pdf
desativadas <- desativadas %>%
  mutate(lon = -53.0286111, lat = -24.18499999)
desativadas
info_fixed <- bind_rows(stns_sul, desativadas) %>%
  # fix station name
  mutate(name = stringr::str_to_title(name),
         state = ordered(state, levels = c("PR", "SC", "RS")))
#DT::datatable(tail(tar_info))
tail(info_fixed)
View(info_fixed)

nrow(info_fixed)
saveRDS(info_fixed, file = "data/info-inmet-sul.rds")

# check sites
sites_data <- readRDS("data/data-raw-inmet-sul-20000922-20161231.rds") %>%
  select(site) %>%
  pull() %>%
  unique()
all(sites_data %in% info_fixed$site)
#all(info_fixed$site %in% sites_data)


