# Author: Daniel A.M. Villela, Rafael Santos de Oliveira and Marlon Mesquita Lopes Cabreira
# functions to implpement path signature in R
# and lasso regression with signatures
#

library("reticulate")

pathsig.env <- function(x) {
  conda_create("r-reticulate")
  use_python("C://Users//User//Documents//.virtualenvs//r-reticulate//Scripts//python.exe")
  #virtualenv_remove(packages = "numpy")
  py_install("numpy<2.0")
  py_install("esig")
  virtualenv_create("r-reticulate")

  # install SciPy
  virtualenv_install("r-reticulate", packages = "numpy")
  # install SciPy
  virtualenv_install("r-reticulate", packages = "esig")

  # import SciPy (it will be automatically discovered in "r-reticulate")
}

getESig <- function(x) {
  esig <- import("esig")
  esig
}

esig <- import("esig")

#pathsig.env()
