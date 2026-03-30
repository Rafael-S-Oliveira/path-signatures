#' Author: Daniel A.M. Villela, Rafael Santos de Oliveira and Marlon Mesquita Lopes Cabreira
#' functions to implpement path signature in R
#' and lasso regression with signatures
#'
#'
#' @import dplyr
#' @import reticulate
# Defina o caminho do conda (substitua pelo seu)
caminho_conda <- "C://Users//marlo//Anaconda//Scripts//conda.exe"
#'
pathsig.env <- function(conda_path = caminho_conda) {
  #
  # Nome do ambiente conda
  env_name <- "r-esig"

  # Verifica se o ambiente já existe, usando o caminho do conda
  if (!condaenv_exists(env_name, conda = conda_path)) {
    conda_create(env_name, python_version = "3.10", conda = conda_path)
  }
  #
  # Instala numpy via conda-forge
  conda_install(env_name,
                packages = "numpy<2.0",
                channel = "conda-forge",
                conda = conda_path)

  # Instala esig via pip dentro do ambiente conda
  conda_install(env_name,
                packages = "esig",
                pip = TRUE,
                conda = conda_path)

  # Aponta o reticulate para usar o Python deste ambiente
  use_condaenv(env_name, conda = conda_path, required = TRUE)
  #
  message("Ambiente configurado com sucesso!")
}
#'
# Executa a função
# pathsig.env()
#'
#'
#'

getESig <- function(x) {
  esig <- import("esig")
  esig
}
#'
# Testa se o esig foi instalado
# print(getESig)

#### setup python ----

#' Configura ambiente Conda com o módulo Python 'esig'
#'
#' Cria (se necessário) um ambiente Conda chamado `"r-esig"`, instala as
#' dependências (`numpy`) via conda-forge e o pacote `esig` via pip.
#' Por fim, aponta o `reticulate` para usar esse ambiente.
#'
#' @param conda_path Caminho para o executável do Conda. Se `NULL` (padrão),
#'   o `reticulate` tentará localizar automaticamente. Recomenda-se informar
#'   o caminho explicitamente para evitar problemas.
#' @param env_name Nome do ambiente Conda a ser usado/criado.
#' @param python_version Versão do Python para o novo ambiente (padrão `"3.10"`).
#' @param force_recria Se `TRUE`, recria o ambiente mesmo se ele já existir.
#'
#' @return Invisivelmente, o resultado de `reticulate::use_condaenv()`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' setup_esig_env(conda_path = "C:/Users/User/Anaconda3/Scripts/conda.exe")
#' }
setup_esig_env <- function(conda_path = NULL,
                           env_name = "r-esig",
                           python_version = "3.10",
                           force_recria = FALSE) {

  # Se o caminho do conda foi fornecido, passa para as funções do reticulate
  conda_arg <- if (!is.null(conda_path)) list(conda = conda_path) else list()

  # Verifica se o ambiente já existe
  if (force_recria || !reticulate::condaenv_exists(env_name, !!!conda_arg)) {
    if (force_recria && reticulate::condaenv_exists(env_name, !!!conda_arg)) {
      message("Removendo ambiente existente para recriação...")
      reticulate::conda_remove(env_name, !!!conda_arg)
    }
    reticulate::conda_create(env_name,
                             python_version = python_version,
                             !!!conda_arg)
  }

  # Instala numpy via conda-forge
  reticulate::conda_install(env_name,
                            packages = "numpy<2.0",
                            channel = "conda-forge",
                            !!!conda_arg)

  # Instala esig via pip
  reticulate::conda_install(env_name,
                            packages = "esig",
                            pip = TRUE,
                            !!!conda_arg)

  # Ativa o ambiente
  reticulate::use_condaenv(env_name, !!!conda_arg, required = TRUE)

  message("Ambiente '", env_name, "' configurado com sucesso!")
}





library(reticulate)
pathsig.env <- function(x) {
  use_condaenv("myenv", required = TRUE)
  # conda_create("r-reticulate")
  #use_python("/usr/bin/python3")
  #virtualenv_remove(packages = "numpy")
  #py_install("numpy<2.0")
  #py_install("esig")
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

#esig <- import("esig")

#pathsig.env()

pathsig.esig <- function(path, m) {
  truncation_level = as.integer(m)
  signature <- esig$stream2sig(path, truncation_level)
  signature
}

pathsig.logsig <- function(path, m) {
  truncation_level = as.integer(m)
  signature <- esig$stream2logsig(path, truncation_level)
  signature
}

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
#path <- array(c(1, 1, 3, 10, 20, 30), c(3, 2))

# Define the truncation level
#truncation_level <- as.integer(5)  # Set this as needed

# Call the stream2sig function
#result <- esig$stream2sig(path, truncation_level)

# Print the result
#print(result)

#pathsig.logsig(path, 5)

#path <- array(c(1,1,3,10,20,30),c(3,2))
#esig$stream2sig(path, truncation_level)

#path
#pathsig.one(path, 2)

#path1 <- array(c(1, 1, 2, 3, 5, 7, 8), c(1,7))
#path1
#pathsig.one(path1, 3)

#path <- array(c(1,1,3,10,20,30),c(2,3))
#path
#pathsig.one(path, 3)

#y <- mtcars$hp

#x <- data.matrix(mtcars[, c("mpg", "wt", "drat", "qsec")])
#x


