#' @title exportar_dados
#' @aliases exportar_dados
#' @author Gabriel Petrini
#' @description Exporta os dados no formato .RData para uma pasta
#' @param dados Objeto a ser exportado
#' @param pasta Caminho para a exportacao
#' @param formato Formato do arquivo final
#' @return Nao modifica o objeto, apenas exporta
#' @export

exportar_dados <- function(dados, pasta = "../Tratados", formato = ".RData"){
    nome_dado <- deparse(substitute(dados))
    nome_arquivo <- paste0(nome_dado, formato)
    caminho <- file.path(pasta, nome_arquivo)
    saveRDS(dados, file = caminho)
}
