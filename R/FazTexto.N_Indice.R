#' @title FazTexto.N_Indice
#' @author Arthur Welle
#' @details Corrigir: Alterar nome
#' @description Cria funcao para criar numero indice de uma serie mensal de inflação (para usar em deflacionamentos até para a ultima data)
#' @param x Serie temporal
#' @return Numero indice
#' @keywords Texto




FazTexto.N_Indice <- function(x)
{
    k <- c(1:length(x))
    k[1] <- (1 + x[1]/100)
    for (i in 2:length(x)) {
        k[i] <- (1 + x[i]/100) * k[i - 1]
    }
    k <- k[length(k)/k]
    return(k)
}
