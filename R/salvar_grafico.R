function (grafico, pasta = "./Graficos", formato = ".png") 
{
    nome_grafico <- deparse(substitute(grafico))
    salvar_nome <- paste0(Sys.Date(), "_", nome_grafico, formato)
    ggsave(file.path(pasta, salvar_nome))
}
