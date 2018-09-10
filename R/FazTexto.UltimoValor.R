#' @title Faz Texto Ultimo Valor
#' @aliases FazTexto.UltimoValor
#' @author Arthur Welle
#' @description Cria uma funcao que retorna o ultimo valor de uma serie x.
#' @export
#' @param x Serie a ser analisada
#' @param digitos Numero de casas decimais. Por padrao, 2
#' @param texto Se TRUE, retorna caractere
#' @return Texto



FazTexto.UltimoValor <- function (x, digitos = 2, texto = TRUE)
{
    if (texto == TRUE) {
    k <- format(round(x[length(x)], digits = digitos), big.mark = ".",
        decimal.mark = ",")
    return(k)
    } else {
        k <- x[length(x)]
        return(k)
    }
}
