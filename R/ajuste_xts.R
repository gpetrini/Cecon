#' @title ajuste_xts
#' @aliases ajuste_xts
#' @author Gabriel Petrini
#' @description Transforma os dados em xts para utilizar nas demais series do pacote
#' @import xts
#' @import dplyr
#' @import zoo
#' @param dados Serie a ser tratada
#' @param col_data Coluna da serie que possui as datas. Por padrao, usa-se a coluna 1
#' @param col_dados Coluna(s) dos dados. Por padrao, seleciona-se a segunda coluna
#' @param remover_NA Se TRUE, remove os linhas que contem NAs. Por padrao, remover_NA = FALSE
#' @return Serie em xts
#' @export

ajuste_xts <- function (dados, col_data = 1, col_dados = 2, remover_NA = FALSE)
{
    if ("xts" %in% class(dados) | "zoo" %in% class(dados)) {
        message("Serie esta em xts/zoo, funcao ajuste_xts nao necessaria")
        if (sum(is.na(dados)) != 0 & remover_NA == FALSE) {
            message("Series contem NAs, use remover_NA = TRUE",
                call. = FALSE)
        }
        if (remover_NA == FALSE) {
            xts(x = coredata(dados), order.by = as.Date(index(dados)))
        }
        else {
            sem_NA <- !is.na(coredata(dados))
            xts(x = coredata(dados[sem_NA]), order.by = as.Date(index(dados[sem_NA])))
        }
    }
    else if ("ts" %in% class(dados)) {
        dados <- as.xts(dados)
    }
    else {
        if (sum(is.na(dados)) != 0 & remover_NA == FALSE) {
            message("Series contem NAs, use remover_NA = TRUE",
                call. = FALSE)
        }
        if (remover_NA == TRUE) {
            teste_NA <- !(is.na(dados[, col_dados]))
            dados_sNA <- dados[teste_NA, ]
            proxy <- as.data.frame(dados_sNA)
            proxy_data <- as.Date(proxy[, col_data], origin = proxy[1,
                col_data])
            xts(x = proxy[, col_dados], order.by = proxy_data)
        }
        else {
            proxy <- as.data.frame(dados)
            proxy_data <- as.Date(proxy[, col_data], origin = proxy[1,
                col_data])
            xts(x = proxy[, col_dados], order.by = proxy_data)
        }
    }
}
