#' Normalização Global de uma Lista de Caminhos
#'
#' Esta função recebe uma lista contendo múltiplos caminhos (matrizes ou data frames),
#' combina-os temporariamente para calcular a média e o desvio padrão globais
#' de cada coluna, e depois padroniza (normaliza) cada caminho individualmente
#' utilizando esses valores globais (Z-score).
#'
#' @param L Uma lista onde cada elemento é uma matriz ou data frame a representar
#'   um caminho. Espera-se que todos os elementos tenham o mesmo número de colunas.
#'
#' @return Uma lista nomeada contendo três elementos: \code{mlist} (a lista original
#'   com os valores normalizados), \code{mmean} (as médias globais calculadas por coluna)
#'   e \code{sd} (os desvios padrões globais calculados por coluna).
#'

doNorm <- function(L) {

  Ll <- length(L)

  # Une toda a lista numa matriz única para encontrar os parâmetros globais
  Ls <- do.call(rbind, L)

  # Calcula a média e o desvio padrão para cada coluna
  mmean <- apply(Ls, 2, mean)
  msd <- apply(Ls, 2, sd)

  # Aplica a normalização (Z-score) em cada elemento da lista original
  mlist = lapply(1:Ll, function(x) {(L[[x]] - mmean)/msd})

  # Retorna os dados normalizados e os parâmetros utilizados
  list(mlist = mlist, mmean = mmean, sd = msd)
}
