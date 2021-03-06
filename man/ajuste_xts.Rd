% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ajuste_xts.R
\name{ajuste_xts}
\alias{ajuste_xts}
\title{ajuste_xts}
\usage{
ajuste_xts(dados, col_data = 1, col_dados = 2, remover_NA = FALSE)
}
\arguments{
\item{dados}{Serie a ser tratada}

\item{col_data}{Coluna da serie que possui as datas. Por padrao, usa-se a coluna 1}

\item{col_dados}{Coluna(s) dos dados. Por padrao, seleciona-se a segunda coluna}

\item{remover_NA}{Se TRUE, remove os linhas que contem NAs. Por padrao, remover_NA = FALSE}
}
\value{
Serie em xts
}
\description{
Transforma os dados em xts para utilizar nas demais series do pacote
}
\author{
Gabriel Petrini
}
