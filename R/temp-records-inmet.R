#' Read maximum and minimum air temperature record from INMET climatological stations 
#'
#' @param file_name full path to file
#'
#' @return data_frame with 
#' @author Jonatan Tatsch
#' @export
#'
#' @examples
#' tmx <- read_temp_records_inmet(file_name = "data-raw/Temperatura-Maxima-Absoluta_NCB_1961-1990.xls")
#' head(tmx); tail(tmx)
#' tmn <- read_temp_records_inmet(file_name = "data-raw/Temperatura-Minima-Absoluta_NCB_1961-1990.xls")
#' head(tmn); tail(tmn)

read_temp_records_inmet <- function(file_name = "data-raw/Temperatura-Minima-Absoluta_NCB_1961-1990.xls"){
  
  is_tmin <- stringr::str_detect(file_name, "Minima")
  if (is_tmin) {
    var_nm <- "tmin_abs" 
    } else {
      var_nm <- "tmax_abs"
    }
# month names and number ------------------------------------------------------
month_names <- c("Janeiro" = 1, 
                 "Fevereiro" = 2,
                 "Março" = 3, 
                 "Abril" = 4, 
                 "Maio" = 5,
                 "Junho" = 6,
                 "Julho" = 7,
                 "Agosto" = 8,
                 "Setembro" = 9,
                 "Outubro" = 10, 
                 "Novembro" = 11,
                 "Dezembro" = 12)

# tidy header from xls file----------------------------------------------------
temp_head <- readxl::read_excel(path = file_name,
                               sheet = 1, 
                               col_names = FALSE,
                               na = "-",
                               skip = 3, 
                               n_max = 2
                               ) %>%
  t() %>%
  tibble::as_data_frame() %>%
  setNames(c("mes", "variable")) %>%
  mutate(variable = ifelse(is.na(variable), "", variable),
         mes = ifelse(is.na(mes), dplyr::lag(mes), mes),
         mes_num = month_names[mes],
         mes_num = ifelse(is.na(mes_num), "", mes_num)) %>%
  unite(var_name, c("variable", "mes_num")) %>%
  mutate(var_name = ifelse(var_name == "_", mes, var_name),
         var_name = ifelse(stringr::str_detect(var_name, "_$"), paste0(var_name, "Ano"), var_name)) %>%
  dplyr::pull() %>%
  str_to_lower() %>%
  stringi::stri_trans_general(., "latin-ascii") %>%
  stringr::str_replace(., " da estacao", "")
#temp_head

# View(temp_head)
#length(temp_head)

# tidy data from xls file------------------------------------------------------
# importa dados das estações meteorológicas convencionais do INMET
# Normais Climatológicas do Brasil 1961-1990
# Temperatura Máxima Absoluta (°C)
temp_data <- readxl::read_excel(path = file_name,
                          sheet = 1, 
                          col_names = FALSE,
                          na = "-",
                          skip = 6
                          ) %>%
  setNames(temp_head) %>%
  # só linhas que comecenm com num. (evitar comentários no final do arquivo)
  dplyr::filter(str_detect(codigo, "^[0-9]")) %>%
  tidyr::gather(variable, value, -(codigo:uf)) %>%
  tidyr::separate(col = variable, into = c("tipo", "mes")) %>%
  tidyr::spread(tipo, value) %>%
  # "ano" foi substituído por zero, logo o valor de 0 refere-se ao máximo anual
  dplyr::mutate(mes = as.integer(ifelse(mes == "ano", "0", mes)),
                valor = as.numeric(valor),
                codigo = as.character(codigo)) %>%
  dplyr::arrange(codigo, mes) %>%
  dplyr::rename(!!var_nm := valor) %>%
  dplyr::rename("site" = codigo,
                "month" = mes)
  
#View(temp_data)
return(temp_data)
}

# ------------------------------------------------------------------------------
#' Join files records of tmax and tmin 
#' Join records of absolute tmax and tmin observed in conventional 
#' meteorological stations of INMET.
#' @param file_tmx path to file of records of maximum air temperature by month
#' @param file_tmn path to file of records of maximum air temperature by month
#'
#' @return
#' @export
#'
#' @examples
temp_records_inmet <- function(file_tmx = "data-raw/Temperatura-Maxima-Absoluta_NCB_1961-1990.xls",
                               file_tmn = "data-raw/Temperatura-Minima-Absoluta_NCB_1961-1990.xls"){
  tmx_records <- read_temp_records_inmet(file_tmx) %>%
    dplyr::select(-(nome:uf), - (ano)) %>% 
    dplyr::filter(month > 0)
    
    tmn_records <- read_temp_records_inmet(file_tmn) %>%
      dplyr::select(-(nome:uf), -(ano)) %>% 
      dplyr::filter(month > 0)
    
    temp_abs <- dplyr::full_join(tmn_records, tmx_records, by = c("site", "month"))
    unique(temp_abs$site)
    #all(unique(table(temp_abs$site)) == 12)
    return(temp_abs)
  
}


# ------------------------------------------------------------------------------
#' Join station metadata and records of temperature 
#' Join conventional stations metada and temperature recods files obtained from INMET's website.
#' @inheritParams temp_records_inmet return details 
#' @author Jonatan Tatsch
#' @return a tibble with columns 
#'  \itemize{
#'    \item \code{site}  
#'    \item \code{mes} 
#'    \item \code{tmin_abs}
#'    \item \code{tmax_abs}
#'    \item \code{lon}
#'    \item \code{lat}
#'    \item \code{alt}
#'    \item \code{name}
#'    \item \code{state}
#'    \item \code{uf}
#'    \item \code{time_zone}
#'    \item \code{offset_utc}
#' }
#' @export
#' @examples
#' meta_emc <- metadata_emc_inmet(file_tmx = "../data-raw/Temperatura-Maxima-Absoluta_NCB_1961-1990.xls",
#'                                file_tmn = "../data-raw/Temperatura-Minima-Absoluta_NCB_1961-1990.xls")
#' meta_emc

metadata_emc_inmet <- function(file_tmx, file_tmn){
  temp_rec <- temp_records_inmet(file_tmx ,file_tmn)
  meta_emc <- inmetr::bdmep_meta %>%
    tibble::as_data_frame() %>%
    dplyr::rename("site" = id)
  dplyr::full_join(temp_rec, meta_emc, by = "site")
}