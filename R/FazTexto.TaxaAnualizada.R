#' @title Faz Texto Taxa Anualizada
#' @aliases FazTexto.TaxaAnualizada
#' @author Arthur Welle
#' @description Cria texto para taxa anualizada
#' @export
#' @param x Serie para calcular taxa anualizada


FazTexto.TaxaAnualizada <- function (x)
{
    k <- c(1:length(x))
    for (i in 0:length(x)) {
        k[i] <- (1 + x[i]/100)^12
    }
    k <- (k - 1) * 100
    return(k)[length(x)]
}
