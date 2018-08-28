#' @title Clean IPEA
#' @aliases clean_ipea
#' @author Gabriel Petrini
#' @description Trata os dados baixados do IPEADATA para serem utilizados com os outros pacotes da serie
#' @import ecoseries
#' @import xts
#' @param dado_ipea Dados baixados do IPEADATA
#' @return Um objeto xts
#' @export


clean_ipeadata <- function (dado_ipea)
{
    dado_desl <- unlist(dado_ipea)
    valor <- dado_desl[((length(dado_desl)/2) + 1):length(dado_desl)]
    data_ipea <- dado_desl[1:(length(dado_desl)/2)]
    data_clean <- as.Date(data_ipea)
    xts_name <- deparse((substitute(dado_ipea)))
    xts(x = valor, order.by = data_clean)
}
