#' Author: Daniel A.M. Villela, Rafael Santos de Oliveira and Marlon Mesquita Lopes Cabreira
#' functions to implement path signature in R
#' and lasso or ridge regression with signatures
#'
#' Calculate Path Signature
#'
#' @param path Numeric matrix representing the path
#' @param m Integer truncation level
#' @param logT Logical; if TRUE compute log-signature
#' @return Numeric vector of signature coefficients
#' @export
#' @examples
#' path <- matrix(c(1,1,3,10,20,30), ncol=2)


library("reticulate")

pathsig.env <- function(x) {
  # conda_create("r-reticulate")
  use_python("/usr/bin/python3")
  #virtualenv_remove(packages = "numpy")
  py_install("numpy<2.0")
  py_install("esig")
  #virtualenv_create("r-reticulate")

  # install SciPy
  #virtualenv_install("r-reticulate", packages = "numpy")
  # install SciPy
  #virtualenv_install("r-reticulate", packages = "esig")

  # import SciPy (it will be automatically discovered in "r-reticulate")
}

getESig <- function(x) {
  esig <- import("esig")
  esig
}

esig <- import("esig")

#pathsig.env()

#' @param path define path array
#' @param m truncation level-integer
#'
#' @examples
#' path <- array(c(1, 1, 3, 10, 20, 30), c(3, 2))
#' pathsig.esig(path,m)
#'
#' @export
pathsig.esig <- function(path, m) {
  truncation_level = as.integer(m)
  signature <- esig$stream2sig(path, truncation_level)
  signature
}
