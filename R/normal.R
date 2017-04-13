#' Normal random vectors
#'
#' Generate and do operations on Normal random vectors
#'
#' @param mu Mean \eqn{\mu}
#' @param sigma Standard deviation \eqn{\sigma}
#' @return A Normal random vector
#'
#' @examples
#' x <- normal(1:10, 10)
#' x
#' mean(x)
#' draw(x)
#'
#' @export
normal <- function(mu, sigma) {
  rv <- complex(real = mu, imaginary = 1 / sigma ^ 2)
  class(rv) <- c("normal", "random")
  rv
}

#' @export
print.normal <- function(x, ...) {
  rv <- paste0("N(", Re(x), ", ", 1 / sqrt(Im(x)), ")")
  print.default(rv, quote = FALSE)
  invisible(x)
}

#' @export
mean.normal <- function(x, ...) {
  as.double(Re(x))
}

#' @importFrom stats rnorm
#' @export
draw.normal <- function(rv) {
  dr <- as.double(Re(rv))
  dr[!is.infinite(Im(rv))] <-
    rnorm(length(rv), Re(rv), 1 / sqrt(Im(rv)))
  dr
}
