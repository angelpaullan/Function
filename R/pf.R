#' @title Cumulative Distribution Function of the Pf Distribution
#'
#' @description
#' Evaluates the cumulative distribution function (CDF) of the Pf
#' continuous distribution at given values of \code{x}.
#'
#' @param x A numeric value or vector where the CDF is evaluated.
#'
#' @return A numeric vector with the corresponding cumulative probabilities.
#'
#' @details
#' The Pf distribution is defined on the interval [0, 2]. Its
#' cumulative distribution function is:
#'
#' \deqn{
#' F(x) =
#' \begin{cases}
#' 0, & x \leq 0 \\
#' \frac{3}{16}x^4 - x^3 + \frac{3}{2}x^2, & 0 < x < 2 \\
#' 1, & x \geq 2
#' \end{cases}
#' }
#'
#' This function is vectorized and returns values in the interval [0, 1].
#'
#' @examples
#' pf(0)
#' pf(c(0, 0.5, 1, 2))
#'
#' @export
pf <- function(x) {
  ifelse(x <= 0, 0,
         ifelse(x < 2, (3/16) * x^4 - x^3 + (3/2) * x^2, 1))
}
