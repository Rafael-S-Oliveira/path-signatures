#' Ajuste de Modelo Lasso Bayesiano com Assinaturas de Caminho
#'
#' Esta função implementa uma abordagem sofisticada e bayesiana para o modelo
#' preditivo. Ela extrai as assinaturas de caminho, normaliza os dados e ajusta
#' um Modelo Linear Generalizado Bayesiano com uma distribuição a priori Lasso,
#' utilizando o pacote \code{rstanarm}.
#'
#' @param L Lista ou matriz contendo os caminhos originais (séries temporais).
#' @param y Vetor contendo a variável resposta (target) que o modelo deve prever.
#' @param m Número inteiro indicando o nível de truncamento da assinatura.
#' @param Ltype String indicando o tipo de entrada (\code{"list"} por padrão).
#' @param family String indicando a família do modelo GLM (ex: \code{"gaussian"}, \code{"binomial"}).
#' @param norm Booleano. Se \code{TRUE} (padrão), normaliza as assinaturas geradas antes da modelagem.
#'
#' @return Uma lista contendo o modelo bayesiano ajustado (\code{model} e \code{cvmodel}),
#'   valores de referência para lambda e coeficientes, a matriz de assinaturas (\code{sig}),
#'   e as médias (\code{meanv}) e desvios padrões (\code{sdv}) usados na normalização.
#'
#' @import rstanarm
#' @export
lasso.pathsig.bayesian <- function(L, y, m, Ltype = "list", family = "gaussian", norm = TRUE) {

  x <- pathsig(L, Ltype, m)

  if (norm) {
    meanv = apply(x,2,mean)
    sdv = apply(x,2,sd)
  } else {
    meanv = 0
    sdv = 1
  }

  df1 = data.frame((x-meanv)/sdv, y=y)

  interface = "NEW"

  out <- rstanarm::stan_glm(y ~ ., data = df1, family = family, prior = lasso(),
                            chains = 3, iter = 1000)

  best_lambda = 1
  coef = 1

  list(lambda = best_lambda, coef = coef, model = out,
       cvmodel = out, sig = x, meanv = meanv, sdv = sdv)
}
