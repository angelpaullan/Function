#' @title Random pseudo-numbers generator from a quartic CDF
#'
#' @description
#' Generates pseudo-random numbers from a continuous distribution defined by
#' a quartic cumulative distribution function (CDF), using the inverse transform method.
#'
#' @param n Integer. Length of the pseudo-random vector to be generated.
#'
#' @details
#' This function applies the inverse transform method:
#' \deqn{F(x) = \tfrac{3}{2}x^2 - x^3 + \tfrac{3}{16}x^4, \quad x \in [0,2]}
#'
#' For each uniform random number \eqn{u \sim U(0,1)}, the algorithm solves
#' \deqn{F(x) = u}
#' numerically using Brent's method (via \code{uniroot}) to obtain the corresponding
#' random variable \eqn{x}.
#'
#' The associated probability density function (PDF) is:
#' \deqn{f(x) = F'(x) = 3x - 3x^2 + \tfrac{3}{4}x^3}
#'
#' @return
#' A numeric vector of length \code{n} with random values following the distribution
#' defined by the quartic CDF above.
#'
#' @examples
#' # Generate 100 random values
#' randomfun(100)
#'
#' @export
randomfun <- function(n) {
  u <- runif(n)
  sapply(u, function(u) uniroot(function(x) (3/2)*x^2 - x^3 + (3/16)*x^4 - u,
                                interval = c(0, 2))$root)
}
