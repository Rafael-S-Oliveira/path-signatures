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
#' @param path define path array
#' @param m truncation level-integer
#'
#' @examples
#' path <- array(c(1, 1, 3, 10, 20, 30), c(3, 2))
#' pathsig.logsig(path,m)
#'
#' @export
pathsig.logsig <- function(path, m) {
  truncation_level = as.integer(m)
  signature <- esig$stream2logsig(path, truncation_level)
  signature
}
#' @param path define path array
#' @param m truncation level-integer
#'
#' @examples
#' path <- array(c(1, 1, 3, 10, 20, 30), c(3, 2))
#' pathsig.one(path,m)
#' log = TRUE or FALSE
#'
#' @export
pathsig.one <- function(path, m, logT = TRUE) {

  if (logT) {
    ssig <- pathsig.logsig(path, m)
  } else {
    ssig <- pathsig.esig(path, m)
  }
  ssig
}

#library(reticulate)

# Import the esig module
#esig <- import("esig")

# Define the path array
# few examples
path <- array(c(1, 1, 3, 10, 20, 30), c(3, 2))

# Define the truncation level
truncation_level <- as.integer(5)  # Set this as needed

# Call the stream2sig function
result <- esig$stream2sig(path, truncation_level)

# Print the result
print(result)

pathsig.logsig(path, 5)

path <- array(c(1,1,3,10,20,30),c(3,2))
esig$stream2sig(path, truncation_level)

path
pathsig.one(path, 2)

path1 <- array(c(1, 1, 2, 3, 5, 7, 8), c(1,7))
path1
pathsig.one(path1, 3)

path <- array(c(1,1,3,10,20,30),c(2,3))
path
pathsig.one(path, 3)

y <- mtcars$hp

x <- data.matrix(mtcars[, c("mpg", "wt", "drat", "qsec")])
x
