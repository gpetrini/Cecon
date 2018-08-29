#' @title Faz Texto Cortes 1
#' @author Arthur Welle
#' @export
#' @description Cria uma função que retorna texto “positivo” “negativo” dependendo se acima ou abaixo de z
#' @param x Serie a ser analisada
#' @param z Parâmetro a ser comparado
#' @param y Se 1, retorna uma palavra feminina. Se 2, retorna masculino


FazTexto.Cortes1 <- function (x, z, y)
{
    if (x >= z & y == 1) {
        k <- "positiva"
    }
    if (x >= z & y == 2) {
        k <- "positivo"
    }
    if (x < z & y == 1) {
        k <- "negativa"
    }
    if (x < z & y == 2) {
        k <- "negativo"
    }
    return(k)
}
