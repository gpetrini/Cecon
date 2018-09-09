#' @title Grafico Rapido
#' @aliases grafico_rapido
#' @author Gabriel Petrini
#' @description Gera um grafico interativo com logo do CECON a partir de uma serie conversivel em xts
#' @import ggplot2
#' @import xts
#' @import dplyr
#' @export
#' @param dado Serie conversivel em xts
#' @param remover_NA Se TRUE, remove NAs
#' @return Grafico interativo com logo do CECON


grafico_rapido <- function (dado, remover_NA = FALSE)
{
    grafico <- dado %>% ajuste_xts(remover_NA = remover_NA) %>%
        grafico_padrao %>% add_logo
    return(grafico)
}
