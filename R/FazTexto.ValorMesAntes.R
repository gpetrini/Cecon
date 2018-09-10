#' @title Faz Texto Valor Meses Antes
#' @aliases FazTexto.ValorMesAntes
#' @author Arthur Welle
#' @export
#' @description Retorna Valor de m meses anteriores
#' @param x Serie a ser analisada
#' @param digitos Numero de casas decimais. Por padrao, 2
#' @param meses Numero de meses a serem analisados
#' @param texto Se TRUE, retorna caractere
#' @return Texto

FazTexto.ValorMesAntes <- function (x, digitos = 2, meses = 12, texto = TRUE)
{
    if (texto == TRUE) {
    k <- format(round(x[length(x) - (meses - 1)], digits = digitos),
        big.mark = ".", decimal.mark = ",")
    return(k)
    } else {
        k <- x[length(x) - (meses - 1)]
        return(k)
    }
}
