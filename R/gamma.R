#' Gamma Random Vectors
#'
#' Generate and do operations on Gamma random vectors
#'
#' @param a Number of events \eqn{\alpha}
#' @param b Time elapsed \eqn{\beta}
#' @return A Gamma random vector
#'
#' @examples
#' x <- gamma(1:10, 10)
#' x
#' mean(x)
#' draw(x)
#'
#' @export
gamma <- function(a, b) {
  rv <- complex(real = a / b, imaginary = b)
  class(rv) <- c("gamma", "random")
  rv
}

#' @export
print.gamma <- function(x, ...) {
  rv <- paste0("G(", Re(x) * Im(x), ", ", Im(x), ")")
  print.default(rv, quote = FALSE)
  invisible(x)
}

#' @export
mean.gamma <- function(x, ...) {
  as.double(Re(x))
}

#' @importFrom stats rgamma
#' @export
draw.gamma <- function(rv) {
  dr <- as.double(Re(rv))
  dr[!is.infinite(Im(rv))] <-
    rgamma(length(rv), Re(rv) * Im(rv), Im(rv))
  dr
}
