#' Fazer Previsões com Modelos de Assinaturas de Caminho
#'
#' Esta função realiza previsões para novos dados utilizando um modelo previamente
#' ajustado (como os retornados por \code{lasso.pathsig} ou \code{lasso.pathsig.util}).
#' Ela aplica automaticamente a mesma extração de assinaturas e a mesma normalização
#' que foram utilizadas durante a fase de treinamento do modelo.
#'
#' @param object Uma lista contendo o modelo treinado e os parâmetros de
#'   normalização (resultado de \code{lasso.pathsig.util} ou similar).
#' @param new Uma lista ou matriz contendo os novos caminhos para os quais
#'   se deseja fazer a previsão.
#' @param s Valor do parâmetro de penalização lambda a ser usado. Se \code{NULL}
#'   (padrão), utiliza o \code{"lambda.min"} encontrado na validação cruzada do modelo.
#' @param Ltype String indicando o tipo de entrada dos novos dados (\code{"list"} por padrão).
#' @param type String indicando o tipo de previsão a ser retornada (ex: \code{"response"}, \code{"class"}).
#' @param conf Booleano. Se \code{TRUE}, retorna também informações adicionais de
#'   confiança ou erro padrão (caso seja suportado pelo modelo base).
#'
#' @return Se \code{conf = FALSE}, retorna um vetor com os valores preditos.
#'   Se \code{conf = TRUE}, retorna uma lista com as predições (\code{pred})
#'   e as estimativas de erro/confiança (\code{conf}).
#'
#' @import stats
#' @export
predict.pathsig <- function(object, new, s = NULL, Ltype = "list", type = "response", conf = FALSE) {

  model <- object$cvmodel
  m <- object$sigdegree

  x <- pathsig(new, Ltype = Ltype, m = m)
  df1 <- data.frame((x - object$meanv) / object$sdv)

  if (is.null(s)) {
    if (type == "class") {
      y_predicted <- predict(model, s = "lambda.min", newdata = df1, type = type)
    } else {
      y_predicted <- predict(model, s = "lambda.min", newdata = df1, type = type)

      if (conf) {
        y_predicted_conf <- predict(model, s = "lambda.min", newdata = df1,
                                    se.fit = TRUE, type = type)
      }
    }
  } else {
    y_predicted <- predict(model, s = s, newdata = df1, type = type)
  }

  if (conf) {
    list(pred = y_predicted, conf = y_predicted_conf)
  } else {
    y_predicted
  }
}
