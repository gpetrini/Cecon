#' @title Faz Texto Corte 2
#' @aliases FazTexto.Cortes3
#' @author Arthur Welle
#' @description Avalia se a serie é muito ruiom, ruim, boa, muito boa.
#' @export
#' @param x Serie a ser analisada
#' @param y Criterio para ruim
#' @param z Criterio para bom
#' @param w Criterio para muito bom
#' @return Texto

FazTexto.Cortes3 <-  function (x, y, z, w)
{
    k <- "muito ruim"
    if (x > y) {
        k <- "ruim"
    }
    if (x > z) {
        k <- "bom"
    }
    if (x > w) {
        k <- "muito bom"
    }
    return(k)
}
