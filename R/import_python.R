# Author: Daniel A.M. Villela, Rafael Santos de Oliveira and Marlon Mesquita Lopes Cabreira
# functions to implpement path signature in R
# and lasso regression with signatures
#
#
#
#
# setup python
#' Setup Conda environment
#'
#' Configures a Conda environment with the Python module 'esig'
#' Creates, if necessary, a Conda environment called "r-esig"`, installs the
#' dependencies numpy via conda-forge and the esig package via pip.
#' Finally, points reticulate to use this environment.
#' @param conda_path Path to the Conda executable. If NULL default,
#' reticulate will try to locate it automatically. It is recommended to specify
#' the path explicitly to avoid problems.
#' @param env_name Name of the Conda environment to be used/created.
#' @param python_version Python version for the new environment default "3.10".
#' @param force_recreate If TRUE, recreates the environment even if it already exists.
#'
#' @return Invisibly, the result of reticulate::use_condaenv.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' setup_esig_env(conda_path = "C:/Users/User/Anaconda3/Scripts/conda.exe")
#' }
#' @import reticulate
#' @import dplyr
setup_esig_env <- function(conda_path = NULL,
                           env_name = "r-esig",
                           python_version = "3.10",
                           force_recria = FALSE) {

  # If the path of the conda was provided, it proceeds to the reticulate functions.
  conda_arg <- if (!is.null(conda_path)) list(conda = conda_path) else list()

  #  Check if the environment already exists.
  if (force_recria || !reticulate::condaenv_exists(env_name, !!!conda_arg)) {
    if (force_recria && reticulate::condaenv_exists(env_name, !!!conda_arg)) {
      message("Removing existing environment for recreation...")
      reticulate::conda_remove(env_name, !!!conda_arg)
    }
    reticulate::conda_create(env_name,
                             python_version = python_version,
                             !!!conda_arg)
  }

  # Install NumPy via conda-forge.
  reticulate::conda_install(env_name,
                            packages = "numpy<2.0",
                            channel = "conda-forge",
                            !!!conda_arg)

  # Install esig via pip
  reticulate::conda_install(env_name,
                            packages = "esig",
                            pip = TRUE,
                            !!!conda_arg)

  # Activate the environment
  reticulate::use_condaenv(env_name, !!!conda_arg, required = TRUE)

  message("environment '", env_name, "' successfully configured!")
}
#' Calculates the signature of a path using the Python module esig
#'
#' @param data Matrix or data frame where each row is a point on the path.
#' The columns represent the dimensions.
#' @param truncation Maximum depth of the signature default = 2.
#' @param env_name Name of the Conda environment containing the esig module
#' usually the one created by setup_esig_env.
#' @return A numeric vector with the signature coefficients up to the specified depth.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' path <- matrix(c(0, 1, 1, 0, 0, 1), ncol = 2, byrow = TRUE)
#' path_signature(path, truncation = 2)
#' }
#' @import reticulate
path_signature <- function(path, truncation = 2, env_name = "r-esig") {

  # Ensures the environment is active (but does not recreate it if it already exists).
  if (!reticulate::py_module_available("esig")) {
    stop("Module 'esig' is not available. Run setup_esig_env() first.")
  }

  esig <- reticulate::import("esig")

  # Converts data to a numeric array (if it's a data frame).
  if (is.data.frame(path)) {
    dados <- as.matrix(path)
  }

  # Calculates signature
  signature <- esig$sig(path, truncation)

  # Returns as a numeric vector from R.
  as.numeric(signature)
}
