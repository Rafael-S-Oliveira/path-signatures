#' Transformação de Caminho Lead-Lag
#'
#' Esta função aplica a transformação "lead-lag" (avanço-atraso) a um caminho
#' bidimensional. Essa técnica é frequentemente usada no cálculo de assinaturas
#' de caminho para capturar a variação quadrática e entender dependências
#' temporais nos dados. Ela intercala e duplica os pontos criando um caminho em degraus.
#'
#' @param X Uma matriz ou data frame contendo o caminho original. Espera-se
#'   que tenha pelo menos duas colunas (representando as duas dimensões a
#'   serem transformadas).
#'
#' @return Um array (matriz) bidimensional contendo o caminho transformado
#'   com a técnica lead-lag.
#'

leadlag2 <- function(X) {

  x1 <- X[,1]
  y1 <- X[,2]

  r1 <- c(x1[1], rep(x1[-1], each=2))
  r2 <- c(rep(head(y1, -1), each =2), y1[length(y1)])

  array(c(r1, r2), dim=c(length(r1),2))
}
