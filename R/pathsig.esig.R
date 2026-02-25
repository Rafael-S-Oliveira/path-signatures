#' function to implement path signature
#'
#' @param path define path array
#' @param m truncation level-integer
#'
#' @examples
#' path <- array(c(1, 1, 3, 10, 20, 30), c(3, 2))
#' m<-2
#' pathsig.esig(path,m)
#'
#' @import
pathsig.esig <- function(path, m) {
  truncation_level = as.integer(m)
  signature <- esig$stream2sig(path, truncation_level)
  signature
}
