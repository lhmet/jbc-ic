#fazer join com inmetr::bdmep_data
#repetir para tmin
#join das tabs de tmax e tmin abs em um único dataframe.

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
  
  if (stringr::str_detect(file_name, "Minima")) {
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
temp_head
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
  dplyr::rename("site" = codigo)
  
#View(temp_data)
return(temp_data)
}


temp_records_inmet <- function(file_tmx = "data-raw/Temperatura-Maxima-Absoluta_NCB_1961-1990.xls",
                               file_tmn = "data-raw/Temperatura-Minima-Absoluta_NCB_1961-1990.xls"){
  tmx_records <- read_temp_records_inmet(file_tmx) %>%
    dplyr::select(-(nome:uf), - (ano)) %>% 
    dplyr::filter(mes > 0)
  
    
    tmn_records <- read_temp_records_inmet(file_tmn) %>%
      dplyr::select(-(nome:uf), -(ano)) %>% 
      dplyr::filter(mes > 0)
    
    temp_abs <- dplyr::full_join(tmn_records, tmx_records, by = c("site", "mes"))
    unique(temp_abs$site)
    #all(unique(table(temp_abs$site)) == 12)
    return(temp_abs)
  
}