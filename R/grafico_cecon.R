#' @title Grafico CECON
#' @aliases grafico_cecon
#' @author Gabriel Petrini
#' @description Gera grafico interatico com logo do cecon
#' @import ggplot2
#' @import xts
#' @import plotly
#' @import dplyr
#' @param dado_xts Dado em xts
#' @param logo Se TRUE, inclui logo do CECON
#' @param FUN Tipo de função que gera gráfico. Padrao grafico_padrao
#' @param ... Argumentos adicionais para a funcao de grafico
#' @return Grafico interativo em html
#' @export
grafico_cecon <-  function (dado_xts, logo = TRUE, FUN = grafico_padrao, ...)
{
    grafico <- dado_xts %>% FUN(...)
    if (logo == TRUE) {
        grafico_logo <- grafico %>% add_logo()
        return(grafico_logo)
    }
    else {
        return(grafico)
    }
}
