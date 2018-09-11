#' @title Faz Texto Taxa Anualizada
#' @aliases FazTexto.TaxaAnualizada
#' @author Arthur Welle
#' @description Cria texto para taxa anualizada
#' @export
#' @param x Serie para calcular taxa anualizada
#' @param digitos Numero de casas decimais
#' @param tempo Se mensal 12, trimestral 4. Se for xts, ts ou zoo, calcula automaticamente.
#' @param texto Se TRUE retorna caractere


FazTexto.TaxaAnualizada <- function (x, digitos = 2, texto = TRUE, tempo = 12)
{
    if (class(x) == "xts" | class(x) == "ts" | class(x) == "zoo") {
        if ((periodicity(x) %in% "monthly")[6] == TRUE) {
            k <- c(1:length(x))
            for (i in 0:length(x)) {
                k[i] <- (1 + x[i]/100)^12
            }
            k <- (k - 1) * 100
        } else if ((periodicity(x) %in% "quarterly")[6] == TRUE) {
            k <- c(1:length(x))
            for (i in 0:length(x)) {
                k[i] <- (1 + x[i]/100)^4
            }
            k <- (k - 1) * 100
        } else if ((periodicity(x) %in% "yearly")[6] == TRUE) {
            warning("Série já anual")
        }  else {
            warning("Rever Série")
        }
    } else {
    k <- c(1:length(x))
    for (i in 0:length(x)) {
        k[i] <- (1 + x[i]/100)^tempo
    }
    k <- (k - 1) * 100
    }
    if (texto == TRUE) {
    Taxa_Anuali <- format(k[length(x)],
                          big.mark = ".",
                          decimal.mark = ",",
                          digits = digitos)
    return(Taxa_Anuali)
    } else {
        if (class(x) == "xts" | class(x) == "ts" | class(x) == "zoo") {
        return(coredata(k[length(x)]))
        } else {
        return(k[length(x)])
        }
    }
}
