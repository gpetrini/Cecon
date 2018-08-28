#' Grafico Padrao
#' @author Gabriel Petrini
#' @description Gera um grafico com um template basico
#' @import xts
#' @import purrr
#' @import ggplot2
#' @importMethodsFrom xts
#' @param dado_xts Objeto em xts ou que possa ser convertido para xts pela funçao ajuste_xts()
#' @param tipo_grafico Tipo de grafico a ser plotado. Por padrao, grafico de linha
#' @param titulo Caractere que especifica qual o titulo do grafico
#' @param fonte Caractere que espeficia a fonte. Nao e necessario escrever "Fonte: "
#' @param x_titulo Caractere com titulo do eixo x
#' @param y_titulo Caractere com titulo do eixo y
#' @param tema Tema do grafico. Por padrao, utiliza-se theme_classic()
#' @param quebra_data Periodo para seccionar o eixo x. Por padrao, e dividido por ano, isto e, "1 year"
#' @param label_data Formato em que e disponibilizada as datas do eixo x. Por padrao, e apresentada em anos, isto e, "%Y"
#' @param pontos Argumento para especificar se devem conter pontos no grafico. Por padrao, nao apresenta pontos, isto e, pontos = 0
#' @example
#' library(astsa)
#' grafico_padrao(unemp)
#' @return Retorna um grafico ggplot
#' @export

grafico_padrao <- function (dado_xts, tipo_grafico = geom_line(size = 1), titulo = NULL,
          fonte = NULL, x_titulo = NULL, y_titulo = NULL, tema = theme_classic(),
          quebra_data = "1 year", label_data = "%Y", pontos = 0){
    dado_xts <- as.xts(dado_xts)
    ggplot(data = dado_xts,
           aes(x = as.Date(index(dado_xts)),
               y = coredata(dado_xts))) +
        tipo_grafico + labs(x = x_titulo,
                            y = y_titulo,
                            title = titulo,
                            caption = paste0("Fonte: ", fonte)) +
        tema + theme(panel.border = element_blank(),
                     axis.line = element_line(colour = "black", size = 0.7),
                     axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5, size = 14),
                     axis.text.y = element_text(angle = 0, hjust = 0, vjust = 0.5, size = 14),
                     text = element_text(size = 10, family = "TT Times New Roman")) +
        scale_x_date(date_breaks = quebra_data,
                     date_labels = label_data) +
        geom_point(size = pontos)
}
