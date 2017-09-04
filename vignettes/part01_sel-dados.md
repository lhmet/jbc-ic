# Seleção do período de análise dados das EMA do INMET
Jônatan Tatsch  
`r format(Sys.time(), '%d %B, %Y')`  


# Introdução 

Este documento relata os critérios utilizados para definição do período de dados para aplicação dos testes de controle de qualidade da temperatura do ar. Como critério inicial serão selecionadas estações meteorológicas automáticas (EMA) dos locais com pelo menos 4 anos de dados (podem ser descontínuos). 

As etapas para seleção do período são:

- carregar os dados

- regularizar as séries temporais de cada EMA para garantir que todas tenham 24 h em cada dia e 365 (ou 366 dias, se ano bissexto) em cada ano

- determinar o período de dados de cada EMA (n° de anos)

- quantificar o número de dados válidos (não faltantes) para cada EMA


# Pacotes, funções e pré-configurações

Carregando pacotes necessários.


```r
# limpa espaço de trabalho
rm(list = ls()) 
# carrega pacotes necessários
pacotes <- c("knitr", "tidyverse", "lubridate", "openair", "doBy")
easypackages::libraries(pacotes)
```

```
## Loading required package: knitr
```

```
## Loading required package: tidyverse
```

```
## Loading tidyverse: ggplot2
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
## Loading tidyverse: dplyr
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## filter(): dplyr, stats
## lag():    dplyr, stats
```

```
## Loading required package: lubridate
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```
## Loading required package: openair
```

```
## Loading required package: doBy
```

```
## All packages loaded successfully
```

```r
# configura chuncks

# carrega scripts necessários
source("../R/gaps.R")
```

Os dados são armazenados usando o Tempo Universal Coordenado [UTC](https://pt.wikipedia.org/wiki/Tempo_Universal_Coordenado). Portanto, podemos definir esse horário como padrão nessa sessão do R. Assim qualquer conversão ou operação com datas e horários usando objetos [POSIX](https://stat.ethz.ch/R-manual/R-devel/library/base/html/DateTimeClasses.html) serão realizadas em UTC.


```r
# definindo globalmente tz = "UTC"
Sys.setenv(TZ = "UTC")
```

# Dados



```r
# carregar os metadados (rds com coordenadas e nomes das EMA)
#tar_info <- 
# carregar dados meteorológicos das EMA
#tar_data <- 
```



# Período de dados


```r
tar_periods <- tar_data %>%
  group_by(site) %>%
  dplyr::summarise(sdate = as.Date(min(date)) # data inicial
                   ,edate = as.Date(max(date)) # data final
                   ,period = round((as.numeric(edate-sdate))/365.25, 1)  %>% # período
                    # desagrupando por site
                    ungroup()
```


# Disponibilidade de dados 

Quantificar a quantidade de dados faltantes por EMA.


```r
tar_summary <- tar_data %>%
  group_by(site) %>%
  dplyr::summarise(tmed = round(mean(tair,na.rm = TRUE), 1) # temp média
                   tmax_abs = max(tair, na.rm = TRUE),
                   tmin_abs = min(tair, na.rm = TRUE),
                   dtr_max = tmax-tmin, # amplitude térmica máxima
                   # % de dados faltantes
                   ,missing = round(percent_NA(tair), 1)
                   # tamnho da falha mais longa
                   ,long_gap = max(gaps(tair)[["length"]])
                   # data inicial da falha mais longa
                   ,sdate_lg = date[gaps(tair)[["start"]][which.max(gaps(tair)[["length"]])]] 
  ) %>%
  # desagrupando por site
  ungroup() %>%
  # combinando com a tabela de informações das EMA
  full_join(tar_info, by = "site")

# combinando com a tabela de períodos
tar_summary <- tar_summary %>%
full_join(tar_periods, by = "site") %>%
  arrange(desc(period))
tar_summary
```

Calcular valores medios diarios de tmax, tmin e dtr (amplitude termica).


```r
# medias diarias
tar_daily <- tar_data %>%
  group_by(site, day = as.Date(date)) %>%
  dplyr::summarise(tmax_med = mean(tmax, na.rm = TRUE)
                   ,tmin_med = 
                   ,dtr_med = 
                   ,n_data = sum(!is.na(tair))
                   ) %>%
  ungroup()
tar_daily

# combinando tar_daily com tar_summary
tar_summary <-  tar_summary %>%
                     full_join(tar_daily, by = "site")
tar_summary
```



```r
tar_summary_file <- "tar_summary_inmet_sul.rds"
saveRDS(tar_summary_file, file = file.path("../output", info_summary_file))
```


## Seleção do período

Para caracterizar o regime térmico das estações selecionar EMAs com pelo menos 4 anos de dados.

