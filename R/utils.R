#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Rep method
#'
#' @param x a random vector
#' @param ... passed on to other methods
#'
#' @export
rep.random <- function(x, ...) {
  out_class <- class(x)
  out <- NextMethod()
  structure(out, class = out_class)
}
