#' Transformação de Caminho Time-Joined
#'
#' Esta função recebe um conjunto de dados sequenciais e aplica a transformação
#' matemática "time-joined" (tempo-junto). Ela intercala os valores criando uma
#' estrutura em degraus, o que é um passo de pré-processamento comum antes do
#' cálculo de assinaturas de caminho (path signatures).
#'
#' @param X Um data frame ou matriz representando o caminho original.
#'   Geralmente espera-se que tenha pelo menos duas colunas.
#'
#' @return Um data frame com duas colunas (\code{time} e \code{value}) contendo
#'   o caminho transformado e estruturado para o pacote esig.
#'
#' @examples
#' # Carregando o pacote
#' library(pathsignatures)
#'
#' # Criando um caminho bidimensional simples (3 observações no tempo)
#' Xpath <- matrix(c(2, 3, 4, 5, 5, 6), ncol = 2)
#'
#' # Exibindo o caminho original
#' Xpath
#'
#' # Aplicando a transformação timejoined
#' tj <- timejoined(Xpath)
#'
#' # Visualizando a nova matriz transformada em degraus
#' tj
#'
#' @export
timejoined <- function(X) {
  # Appends the last element of X to the list
  # corrigir <- "ordem transposta "
  X <- rbind(X, X[nrow(X), ])
  l <- list()

  for (j in seq(1, 2 * nrow(X) + 1 + 2)) {
    if (j == 1) {
      l <- rbind(l, c(X[1, 1], 0))
      next
    }
    for (i in seq(1, nrow(X) - 1)) {
      if (j == 2 * i) {
        l <- rbind(l, X[i, ])
        break
      }
      if (j == 2 * i + 1) {
        l <- rbind(l, c(X[i + 1, 1], X[i, 2]))
        break
      }
    }
  }
  l <- as.data.frame(l)
  colnames(l) <- c("time", "value")
  return(l)
}
