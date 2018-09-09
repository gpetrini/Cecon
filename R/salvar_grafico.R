#' @title Salvar Grafico
#' @aliases salvar_grafico
#' @author Gabriel Petrini
#' @description Salvar objeto ggplot em uma pasta especifica
#' @import ggplot2
#' @param grafico Grafico gerado pelo ggplot
#' @param pasta Caminha onde salvar o grafico
#' @param formato Formato a ser salvo
#' @return Imagem na pasta especificada
#' @export

salvar_grafico <- function (grafico, pasta = "./Graficos", formato = ".png")
{
    nome_grafico <- deparse(substitute(grafico))
    salvar_nome <- paste0(Sys.Date(), "_", nome_grafico, formato)
    ggsave(file.path(pasta, salvar_nome))
}
