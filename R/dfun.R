#' @title Probability Density Function
#'
#' @description
#' Computes the probability density function (pdf) of a custom distribution
#' defined on the interval \eqn{[-1,1]}.
#'
#' @usage dfun(x)
#'
#' @param x Numeric vector at which the density is evaluated.
#'
#' @details
#' The density function is given by:
#' \deqn{
#' f(x) = \begin{cases}
#' \frac{35}{32}(1 - x^2)^3 & \text{if } -1 \le x \le 1 \\
#' 0 & \text{otherwise}
#' \end{cases}
#' }
#'
#' This density is symmetric around zero and reaches its maximum at
#' \eqn{x = 0}.
#'
#' The integral of \eqn{f(x)} over \eqn{[-1,1]} equals 1, ensuring that it is
#' a valid probability density function.
#'
#' @return
#' A numeric vector with the density values evaluated at \code{x}.
#'
#' @examples
#' # Evaluate the density at a single point
#' dfun(0)
#'
#' # Evaluate the density at a sequence of points
#' x <- seq(-1, 1, 0.01)
#' plot(x, dfun(x), type = "l", col = "red", lwd = 2,
#'      main = "Density function f(x)", ylab = "f(x)")
#'
#' @export
dfun <- function(x) {
  ifelse(x >= -1 & x <= 1, (35/32) * (1 - x^2)^3, 0)
}
