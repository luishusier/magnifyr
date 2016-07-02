#' Infer Bernoulli process
#'
#' Infer a Bernoulli process from data
#'
#' @param obs A series of Bernoulli trials
#' @return a Beta random variable for the probability \eqn{p}
#'
#' @export
infer_bernoulli <- function(obs) {
  beta(sum(obs), length(obs) - sum(obs))
}
