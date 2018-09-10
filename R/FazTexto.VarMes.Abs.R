#' @title Faz Texto Variacao M meses absoluta
#' @aliases FazTexto.VarMes.Abs
#' @author Arthur Welle
#' @export
#' @description Cria uma funcao que retorna a variação absoluta (em pontos percentuais se ja for uma serie percental) de uma serie no ultimo ano
#' @param x Serie percentual
#' @param digitos Numero de casas decimais. Por padrao, 2
#' @param meses Numero de mesesa serem calculados.Por padrao, 12
#' @param texto Se TRUE, retorna valor em caractere
#' @return Texto

FazTexto.VarMes.Abs <- function (x, digitos = 2, meses = 12, texto = TRUE)
{
    if (texto == TRUE) {
    k <- format(round(x[length(x)] - x[length(x) - (meses - 1)],
        digits = digitos), big.mark = ".", decimal.mark = ",")
    return(k)
    } else {
        k <- x[length(x)] - x[length(x) - (meses - 1)]
        return(k)
    }
}
