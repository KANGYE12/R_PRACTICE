#' Probability P(X >= a) for Normal Distribution
#'
#' @description Computes P(X >= a) for a normal distribution using the tail probability.
#' @param a Threshold value.
#' @param mean Mean of the normal distribution.
#' @param std Standard deviation of the normal distribution.
#' @return Probability that X ≥ a.
#' @examples
#' p_normal_greater(5, 6, 1.5)
#' @export
p_normal_greater <- function(a, mean, std) {
  1 - pnorm(a - 1, mean = mean, sd = std)
}


#' Probability P(X >= a) for Poisson Distribution
#'
#' @description Computes P(X >= a) for a Poisson distribution using the tail probability.
#' @param a Threshold integer.
#' @param lambda Poisson rate parameter.
#' @return Probability that X ≥ a.
#' @examples
#' p_poisson_greater(10, 4)
#' @export
p_poisson_greater <- function(a, lambda) {
  1 - ppois(a - 1, lambda)
}


#' Histogram of Poisson Random Samples
#'
#' @description Generates a histogram of Poisson-distributed samples.
#' @param n Sample size.
#' @param lambda Poisson rate parameter.
#' @return A histogram (plot).
#' @examples
#' poisson_histogram(1000, 4)
#' @export
poisson_histogram <- function(n, lambda) {
  data <- rpois(n, lambda)
  hist(
    data,
    main = sprintf("Histogram of Poisson(λ=%s) Samples", lambda),
    xlab = "x",
    col = "skyblue",
    border = "white"
  )
}
