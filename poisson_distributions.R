#' Poisson Distribution P(λ)
#' @family discrete-distributions
#' @description PMF, CDF, and RNG for a Poisson distribution with rate
#parameter λ > 0.
#'
#' @details The Poisson distribution models the number of events occurring in
#a fixed time interval
#' given an average rate λ. Its support is the set {0, 1, 2, ...}.
#'
#' @param k Integer value(s) at which to evaluate the PMF.
#' @param q Numeric thresholds at which to evaluate the CDF.
#' @param lambda Positive numeric rate parameter (λ > 0).
#' @param n Non-negative integer sample size for RNG.
#' @return For `poisson_pmf()` and `poisson_cdf()`: numeric vector of
#probabilities.
#' For `poisson_rnd()`: integer vector of simulated values.
#'
#' @examples
#' poisson_pmf(0:5, 3)
#' poisson_cdf(0:5, 3)
#' poisson_rnd(10, 3)
#'
#' @export
poisson_pmf <- function(k, lambda) {
  if (!is.numeric(lambda) || any(lambda <= 0)) stop("'lambda' must be
positive.")
  k <- as.numeric(k)
  if (any(k < 0 | k %% 1 != 0)) stop("'k' must be non-negative integers.")
  dpois(k, lambda)
}
#' @rdname poisson_pmf
#' @export
poisson_cdf <- function(q, lambda) {
  if (!is.numeric(lambda) || any(lambda <= 0)) stop("'lambda' must be
positive.")
  q <- as.numeric(q)
  ppois(q, lambda)
}
#' @rdname poisson_pmf
#' @export
poisson_rnd <- function(n, lambda) {
  if (!is.numeric(lambda) || any(lambda <= 0)) stop("'lambda' must be
positive.")
  if (!is.numeric(n) || length(n) != 1 || n < 0 || n %% 1 != 0) stop("'n'
must be a non-negative integer.")
  rpois(n, lambda)
}
