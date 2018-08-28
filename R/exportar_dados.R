function (dados, pasta = "../Tratados", formato = ".RData") 
{
    nome_dado <- deparse(substitute(dados))
    nome_arquivo <- paste0(nome_dado, formato)
    caminho <- file.path(pasta, nome_arquivo)
    saveRDS(dados, file = caminho)
}
