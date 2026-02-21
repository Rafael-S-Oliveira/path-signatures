#' function to implement log path signature
#'
#' @param path define path array
#' @param m truncation level-integer
#'
#' log-signature
#'
#' @examples
#' path <- array(c(1, 1, 3, 10, 20, 30), c(3, 2))
#' m<-2
#' pathsig.logsig(path,m)
#'
#' @export
pathsig.logsig <- function(path, m) {
  truncation_level = as.integer(m)
  signature <- esig$stream2logsig(path, truncation_level)
  signature
}
