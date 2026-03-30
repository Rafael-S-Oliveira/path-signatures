#' Aplicação em Lote de Assinaturas de Caminho
#'
#' Esta função atua como um facilitador (wrapper) para aplicar o cálculo de
#' assinaturas de caminho (\code{pathsig.one}) a múltiplos caminhos de uma só vez.
#' Ela pode processar caminhos que estão armazenados em uma lista ou nas colunas
#' de uma matriz/data frame.
#'
#' @param L A estrutura de dados contendo os caminhos. Pode ser uma lista (onde
#'   cada elemento é um caminho) ou uma matriz.
#' @param Ltype Uma string de texto indicando o tipo de entrada. Se for \code{"list"},
#'   a função itera sobre os elementos da lista. Caso contrário, itera sobre as colunas.
#' @param m Um número inteiro (\code{integer}) que define o nível de truncamento
#'   (truncation level) para o cálculo da assinatura.
#'
#' @return Uma matriz transposta onde cada linha corresponde à assinatura
#'   calculada de um dos caminhos fornecidos na entrada.
#'

pathsig <- function(L, Ltype, m) {

  lL <- length(L)
  if (Ltype == "list") {
    X <- sapply(1:lL, function(x) { pathsig.one(L[[x]], m) })
  }
  else {
    X <- sapply(1:lL, function(x) { pathsig.one(L[,x], m) })
  }
  x <- t(data.matrix(X))
  x
}
