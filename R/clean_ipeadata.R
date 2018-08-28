function (dado_ipea) 
{
    dado_desl <- unlist(dado_ipea)
    valor <- dado_desl[((length(dado_desl)/2) + 1):length(dado_desl)]
    data_ipea <- dado_desl[1:(length(dado_desl)/2)]
    data_clean <- as.Date(data_ipea)
    xts_name <- deparse((substitute(dado_ipea)))
    xts(x = valor, order.by = data_clean)
}
