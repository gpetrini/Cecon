#' @title Faz Texto Acumulado Mes
#' @aliases FazTexto.AccMes
#' @author Arthur Welle
#' @export
#' @description Gera texto com taxa acumulada em m meses
#' @return Texto
#' @param x Serie a ser calculada a taxa
#' @param digitos Numero de casas decimais. Por padrao, 2
#' @param meses Numero de meses para acumular. Por padrao, 12 meses

FazTexto.AccMes <- function (x, digitos = 2, meses = 12)
{
    k <- 0
    for (i in 0:(meses - 1)) {
        k <- k + x[length(x) - i]
    }
    k <- format(round(k, digits = digitos), big.mark = ".", decimal.mark = ",")
    return(k)
}
