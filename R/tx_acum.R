#' @title Taxa Acumulada
#' @aliases tx_acum
#' @author Arthur Welle
#' @description Retorna taxa acumulada em m meses
#' @return Um objeto xts
#' @import xts
#' @param dados Objeto conversivel em xts
#' @param digitos Numero de casas decimais. Por padrao, 2
#' @param meses Numero de meses para acumular a taxa de crescimento
#' @param tabela Se TRUE, retorna a taxa acumulada junto com os dados originais
#' @import xts
#' @keywords Transformacoes

tx_acum <- function (dados, digitos = 2, meses = 12, tabela = FALSE)
{
    if ("ts" %in% class(dados)) {
        dados <- as.xts(dados)
    }
    else if ("tbl_df" %in% class(dados) | "tbl" %in% class(dados)) {
        stop("Usar ajuste_xts", call. = FALSE)
    }
    else if ("xts" %in% class(dados) | "zoo" %in% class(dados)) {
        dados <- dados
    }
    else {
        stop("Usar ajuste_xts", call. = FALSE)
    }
    m <- c(1:(length(dados)))
    for (i in 1:(length(dados) - (meses - 1))) {
        k <- 1
        for (j in 0:(meses - 1)) {
            k <- k * (1 + dados[i - j + (meses - 1)]/100)
        }
        k <- (k - 1) * 100
    }
    m[i + (meses - 1)] <- k
    m[1:(meses - 1)] <- NA
    return(m)
    if (tabela == FALSE) {
        acum_xts <- xts(x = k, order.by = as.Date(index(dados)))
        return(acum_xts)
    }
    else {
        acum_xts <- xts(x = k, order.by = as.Date(index(dados)))
        Tabela_accum <- merge.xts(x = dados, y = acum_xts)
        return(Tabela_accum)
    }
}
