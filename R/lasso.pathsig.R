#' Ajuste Padrão de Modelo Lasso com Assinaturas de Caminho
#'
#' Esta função ajusta um Modelo Linear Generalizado com penalização Lasso
#' utilizando as assinaturas de caminho extraídas dos dados originais.
#' Possui suporte explícito e otimizado para a família 'multinomial' (classificação
#' com múltiplas categorias), aplicando agrupamento aos coeficientes.
#'
#' @param L Lista ou matriz contendo os caminhos originais (séries temporais).
#' @param y Vetor contendo a variável resposta (target).
#' @param m Número inteiro indicando o nível de truncamento da assinatura.
#' @param Ltype String indicando o tipo de entrada (\code{"list"} por padrão).
#' @param family String indicando a família do modelo GLM (ex: \code{"gaussian"}, \code{"multinomial"}).
#'
#' @return Uma lista contendo o melhor valor de lambda (\code{lambda}), os coeficientes (\code{coef}),
#'   o modelo final ajustado (\code{model}), o modelo de validação cruzada (\code{cvmodel}),
#'   e a matriz de assinaturas gerada (\code{sig}).
#'
#' @import glmnet
#' @export
lasso.pathsig <- function(L, y, m, Ltype = "list", family = "gaussian") {

  x <- pathsig(L, Ltype, m)

  if (family == "multinomial") {
    cv.model <- cv.glmnet(x, y, alpha = 1, type.multinomial = "grouped")
  } else {
    cv.model <- cv.glmnet(x, y, alpha = 1)
  }

  best_lambda <- cv.model$lambda.min

  if (family == "multinomial") {
    best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda,
                         family = family,
                         type.multinomial = "grouped")
  } else {
    best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda,
                         family = family)
  }

  coef <- coef(best_model)

  list(lambda = best_lambda, coef = coef, model = best_model, cvmodel = cv.model, sig = x)
}
