#' function to implement path signature or log path signature
#'
#' @param path define path array
#' @param m truncation level-integer
#' @param logT = TRUE (log-signature) or logT = FALSE
#'
#' @examples
#' path <- array(c(1, 1, 3, 10, 20, 30), c(3, 2))
#' m<-2
#' pathsig.one(path,m, logT= TRUE)
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
