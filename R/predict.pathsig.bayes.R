#' Fazer Previsões com Modelos Bayesianos de Assinaturas de Caminho
#'
#' Esta função realiza previsões para novos dados utilizando um modelo bayesiano
#' previamente ajustado (como o retornado por \code{lasso.pathsig.bayesian}).
#' Ela aplica a mesma extração de assinaturas e normalização, e utiliza a função
#' \code{posterior_predict} para gerar amostras da distribuição preditiva a posteriori.
#'
#' @param object Uma lista contendo o modelo bayesiano treinado e os parâmetros de
#'   normalização (resultado de \code{lasso.pathsig.bayesian}).
#' @param new Uma lista ou matriz contendo os novos caminhos para previsão.
#' @param m Número inteiro indicando o nível de truncamento da assinatura.
#' @param s Valor opcional do parâmetro de penalização. Se \code{NULL} (padrão),
#'   utiliza predição bayesiana completa.
#' @param Ltype String indicando o tipo de entrada dos novos dados (\code{"list"} por padrão).
#' @param type String indicando o tipo de previsão a ser retornada (\code{"response"} por padrão).
#'
#' @return Uma matriz ou vetor de valores preditos (amostras a posteriori) para os novos dados.
#'
#' @import rstanarm
#' @import stats
#' @export
predict.pathsig.bayes <- function(object, new, m, s = NULL, Ltype = "list", type = "response") {

  model <- object$cvmodel

  x <- pathsig(new, Ltype = Ltype, m = m)
  df1 <- data.frame((x - object$meanv) / object$sdv)

  if (is.null(s)) {
    y_predicted <- posterior_predict(model, newdata = df1, type = type)
  } else {
    y_predicted <- predict(model, s = s, newx = x, type = type)
  }

  y_predicted
}
