#' @title Faz Texto Variacao m meses percentual
#' @aliases FazTexto.VarMes.porc
#' @author Arthur Welle
#' @export
#' @description Cria uma funcao que retorna a variacao percentuais de uma serie nos ultimos m meses
#' @param x Serie a ser analisada
#' @param digitos Numero de casas decimias. Por padrao, 2
#' @param meses Numero de meses a serem calculados. Por padrao, 12
#' @return Texto



FazTexto.VarMes.porc <- function (x, digitos = 2, meses = 12)
{
    k <- format(round((((x[length(x)]/x[length(x) - (meses -
        1)]) - 1) * 100), digits = digitos), big.mark = ".",
        decimal.mark = ",")
    k <- paste0(k, "%")
    return(k)
}
