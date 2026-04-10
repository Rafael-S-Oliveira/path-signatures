#' Ajuste de Modelo Lasso com Assinaturas de Caminho
#'
#' Esta função é o motor principal do pipeline preditivo. Ela recebe uma lista de
#' caminhos e uma variável resposta, extrai as assinaturas de caminho, aplica
#' normalização e ajusta um Modelo Linear Generalizado com penalização Lasso
#' (usando validação cruzada para encontrar o hiperparâmetro ótimo lambda).
#'
#' @param L Lista ou matriz contendo os caminhos originais (séries temporais).
#' @param y Vetor contendo a variável resposta (target) que o modelo deve prever.
#' @param m Número inteiro indicando o nível de truncamento da assinatura.
#' @param Ltype String indicando o tipo de entrada (\code{"list"} por padrão).
#' @param family String indicando a família do modelo GLM (ex: \code{"gaussian"}, \code{"binomial"}).
#' @param norm Booleano. Se \code{TRUE} (padrão), normaliza as assinaturas geradas antes da modelagem.
#'
#' @return Uma lista abrangente contendo o modelo Lasso final ajustado (\code{model}),
#'   os coeficientes (\code{coef}), o melhor lambda (\code{lambda}), o modelo de
#'   validação cruzada (\code{cvmodel}), o grau de truncamento (\code{sigdegree}),
#'   a matriz de assinaturas (\code{sig}), e as médias (\code{meanv}) e desvios
#'   padrões (\code{sdv}) usados na normalização.
#'
#' @import glmnet
#' @import glmnetUtils
#' @export
lasso.pathsig.util <- function(L, y, m, Ltype = "list", family = "gaussian", norm = TRUE) {

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

  if (interface != "OLD") {
    cv.model <- glmnetUtils::cv.glmnet(y ~ ., alpha=1, family = family, data = df1)
  } else {
    cv.model <- cv.glmnet(x, y, alpha=1)
  }

  best_lambda <- cv.model$lambda.min

  if (interface != "OLD") {
    best_model = glmnetUtils::glmnet(y ~ ., family = family, data=df1, alpha = 1)
  } else {
    best_model <- glmnet(x,y, alpha = 1, lambda = best_lambda,
                         family = family)
  }

  coef = coef(best_model)

  list(lambda = best_lambda, coef = coef, model = best_model,
       sigdegree = m,
       cvmodel = cv.model, sig = x, meanv = meanv, sdv = sdv)
}
