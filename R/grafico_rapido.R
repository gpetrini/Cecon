function (dado, remover_NA = FALSE) 
{
    grafico <- dado %>% ajuste_xts(remover_NA = remover_NA) %>% 
        grafico_padrao %>% add_logo
    return(grafico)
}
