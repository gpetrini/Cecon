avaliacao_rapida <- function(dado){
    Cabecalho <- dado %>% head()
    Rodape <- dado %>% tail()
    Classe <- dado %>% class()
    Resumo <- dado %>% summary()
    Estrutura <- dado %>% str()
    Grafico <- dado %>% plot()
    # if (dado %>% is.na() %>% sum() == 0) {
    #     print("Sem NAs")
    # }
    # else {
    #     print("Cuidado, contém NAs")
    # }
    return(list(Cabecalho, Rodape, Classe, Resumo))
}
