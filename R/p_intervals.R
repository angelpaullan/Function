#' @title Interval Probabilities for Chi-square Test
#'
#' @description
#' Computes theoretical probabilities over subintervals of [0,2]
#' for the custom distribution. These probabilities are used in
#' the Chi-squared goodness-of-fit test.
#'
#' @param breaks Numeric vector of interval boundaries.
#'
#' @details
#' The probabilities are computed using the cumulative distribution function:
#' \deqn{
#' P(x_{i-1} \le X \le x_i) = F(x_i) - F(x_{i-1})
#' }
#'
#' where:
#' \deqn{
#' F(x) = \frac{3}{2}x^2 - x^3 + \frac{3}{16}x^4
#' }
#'
#' @return
#' A numeric vector of probabilities for each interval.
#'
#' @examples
#' breaks <- seq(0, 2, by = 0.2)
#' p_intervals(breaks)
#'
#' @export
p_intervals <- function(breaks) {

  # Función acumulada
  F <- function(x) {
    ifelse(x < 0, 0,
           ifelse(x > 2, 1,
                  (3/2)*x^2 - x^3 + (3/16)*x^4))
  }

  probs <- F(breaks)
  diff(probs)
}
