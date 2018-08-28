function (dado_xts, logo = TRUE, FUN = grafico_padrao) 
{
    grafico <- dado_xts %>% FUN()
    if (logo == TRUE) {
        grafico_logo <- grafico %>% add_logo()
        return(grafico_logo)
    }
    else {
        return(grafico)
    }
}
