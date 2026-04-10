#' Transformação de Caminho Lead-Lag (Produto de Kronecker)
#'
#' Esta é uma implementação alternativa e mais geral da transformação "lead-lag",
#' que utiliza a operação matemática do Produto de Kronecker (\code{\%x\%}) para
#' criar os desdobramentos de avanço (lead) e atraso (lag). Ela prepara matrizes
#' e arrays multidimensionais para a extração de assinaturas de caminho.
#'
#' @param X Uma matriz ou array representando o caminho original.
#'
#' @return Uma matriz onde as colunas originais são duplicadas e deslocadas,
#'   retornando a estrutura combinada de avanço e atraso.
#'

leadlag <- function(X) {

  dim1 = dim(X)[1]
  dim2 = dim(X)[2]

  # Aplica o Produto de Kronecker para criar os atrasos
  rlag <- rbind(X[-dim1,]  %x% c(1,1), X[dim1,])

  # Aplica o Produto de Kronecker para criar os avanços
  rlead <- rbind(X[1,], X[-1,]  %x% c(1,1))

  # Combina as duas transformações
  a <- cbind(rlead, rlag)

  return(a)
}
