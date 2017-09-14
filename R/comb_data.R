rm(list = ls()) 
# carrega pacotes necessários
pacotes <- c("knitr", "tidyverse", "lubridate", "openair", "stringr", 
             "magrittr", "padr")
easypackages::libraries(pacotes)

source("../R/gaps.R")
source("../R/utils.R")
source("../R/complete_dates.R")

# teste de combinação dos dados
tar_info <- readRDS("../data/info-inmet-sul.rds")
tar_info


# carregar dados meteorológicos das EMA
tar_data <- readRDS("../data/data-raw-inmet-sul-20000922-20161231.rds")
tar_data <- as_data_frame(tar_data )
gc()
glimpse(tar_data)

x <- readRDS("../data/dataSel_inmet_sul_localtime.rds")
x
