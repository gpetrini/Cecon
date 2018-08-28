function (dados, digitos = 2, meses = 12, tabela = FALSE) 
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
    y <- c(1:(length(dados)))
    for (i in 1:(length(dados) - (meses - 1))) {
        y[i + (meses - 1)] <- mean(dados[(i + 0):(i + (meses - 
            1))])
    }
    y[1:meses] <- NA
    if (tabela == FALSE) {
        mm_xts <- xts(x = y, order.by = as.Date(index(dados)))
        return(mm_xts)
    }
    else {
        mm_xts <- xts(x = y, order.by = as.Date(index(dados)))
        Tabela_mm <- merge.xts(x = dados, y = mm_xts)
        return(Tabela_mm)
    }
}
