#' Draw from a random vector
#'
#' @param rv A random vector
#' @return A numeric vector of same length
#'
#' @export
draw <- function(rv) {
  UseMethod("draw")
}
