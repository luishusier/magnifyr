#' Beta Random Vectors
#'
#' Generate and do operations on Beta random vectors
#'
#' @param a Number of successes \eqn{\alpha}
#' @param b Number of failures \eqn{\beta}
#' @return A Beta random vector
#'
#' @examples
#' x <- beta(1:10, 10:1)
#' x
#' mean(x)
#' draw(x)
#'
#' @export
beta <- function(a, b) {
  rv <- complex(real = a / (a + b), imaginary = a + b)
  class(rv) <- c("beta", "random")
  rv
}

#' @export
print.beta <- function(x, ...) {
  rv <- paste0("B(", Re(x) * Im(x), ", ", Im(x) - Re(x) * Im(x), ")")
  print.default(rv, quote = FALSE)
  invisible(x)
}

#' @export
mean.beta <- function(x, ...) {
  as.double(Re(x))
}

#' @importFrom stats rbeta
#' @export
draw.beta <- function(rv) {
  dr <- as.double(Re(rv))
  dr[!is.infinite(Im(rv))] <-
    rbeta(length(rv), Re(rv) * Im(rv), Im(rv) - Re(rv) * Im(rv))
  dr
}
