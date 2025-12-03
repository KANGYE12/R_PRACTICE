#' Moments of the Poisson Distribution P(λ)
#' @description Compute the mean and variance of a Poisson distribution with
#parameter λ.
#' @param lambda Positive rate parameter.
#' @return A list with elements `mean` and `variance`.
#' @examples
#' poisson_moments(3)
#' @export
poisson_moments <- function(lambda) {
  if (!is.numeric(lambda) || any(lambda <= 0)) stop("'lambda' must be
positive.")
  list(mean = lambda, variance = lambda)
}