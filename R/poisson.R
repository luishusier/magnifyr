#' Infer Poisson process
#'
#' Infer a Poisson process from data
#'
#' @param obs A series of Poisson trials
#' @return a Gamma random variable for the rate \eqn{\lambda}
#'
#' @export
infer_poisson <- function(obs) {
  gamma(sum(obs), length(obs))
}
