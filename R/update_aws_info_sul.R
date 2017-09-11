#format(Sys.time(), '%d %B, %Y')
#[1] "11 setembro, 2017"
easypackages::packages(c("inmetdown", "tidyverse", "lubridate"))
stns <- aws_stations()
stns
stns_sul <- filter(stns, state %in% c("RS", "SC", "PR"))
saveRDS(stns_sul, file = "~/Dropbox/github/my_reps/lhmet/jbc-ic/output/tar-info-inmet-update-south.rds")