#Função para determinar nº de valores faltantes em um vetor.
n_NA <- function(x) sum(is.na(x))
percent_NA <- function(x, N) {
  if (missing(N)) return(n_NA(x)/length(x) * 100)

  n_NA(x)/N * 100
}



## Funcao gaps (hydrosanity) com a opcao reverse 
## default: reverse = F
## se reverse = T, retorna a posicao e o numero de elementos a partir daquela posicao com dados validos
gaps <- 
function(x, max.length = Inf, internal.only = F, reverse = F) 
{
   # x <- d$tair
  
    if (reverse) seriesNA <- !is.na(x) else  seriesNA <- is.na(x)
    if (all(seriesNA %in% FALSE)) return(data.frame(length = 0, start = 0))
    if (all(seriesNA %in% TRUE)) return(data.frame(length = length(seriesNA), start = 1))
    
    diffNA <- c(0, diff(seriesNA))
    preDataGap <- match(F, seriesNA) - 1
    postDataGap <- match(F, rev(seriesNA)) - 1
    diffNA[preDataGap + 1] <- 0
    gapEnd <- which(diffNA == -1) - 1
    nGaps <- length(gapEnd)
    naCumSum <- cumsum(seriesNA)
    gapLength <- naCumSum[gapEnd] - naCumSum[c(preDataGap + 1, 
        gapEnd[-nGaps])]
    if (internal.only == FALSE) {
        gapLength <- c(if (preDataGap > 0) {
            preDataGap
        }, gapLength, if (postDataGap > 0) {
            postDataGap
        })
        gapEnd <- c(if (preDataGap > 0) {
            preDataGap
        }, gapEnd, if (postDataGap > 0) {
            length(x)
        })
    }
    ok <- (gapLength <= max.length)
    gapLength <- gapLength[ok]
    gapEnd <- gapEnd[ok]
    gapStart <- gapEnd - gapLength + 1
    gapInfo <- data.frame(length = gapLength, start = gapStart)
    if (internal.only) {
        attr(gapInfo, "pre.data") <- preDataGap
        attr(gapInfo, "post.data") <- postDataGap
    }
    return(gapInfo)
}


longest_gap <- function(x, ...){
  if (all(is.na(x))) return(0)
  # x <- filter(tar_data, site == "A807") %>% select(tair) %>% pull()
  gp <- gaps(x, ...)
  max(gp$length)
}

date_longest_gap <- function(temp, dates, ...){
  # temp <- d$tair; dates <- d$date
  gp <- gaps(temp, ...)
  #if (nrow(gp) == 1 && gp$length == 0) return(NA)
  if (nrow(gp) == 1 & all(gp$length == 0)) return(NA)
  
  sdates_gaps <- dates[gp$start]
  date_longest_gap <- sdates_gaps[which.max(gp$length)[1]]
  return(date_longest_gap)
}
