function (dados, col_data = 1, col_dados = 2, remover_NA = FALSE) 
{
    if ("xts" %in% class(dados) | "zoo" %in% class(dados)) {
        message("Série está em xts/zoo, função ajuste_xts não necessária")
        if (sum(is.na(dados)) != 0 & remover_NA == FALSE) {
            message("Séries contém NAs, use remover_NA = TRUE", 
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
            message("Séries contém NAs, use remover_NA = TRUE", 
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
