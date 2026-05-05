#' @title Probability Density Function
#'
#' @description
#' Computes the probability density function (pdf) of a custom distribution
#' defined on the interval \eqn{[0,2]}.
#'
#' @usage df(x)
#'
#' @param x Numeric vector at which the density is evaluated.
#'
#' @details
#' The density function is given by:
#' \deqn{
#' f(x) = \begin{cases}
#' \tfrac{3}{4} \, x \, (2 - x)^2 & \text{if } 0 \le x \le 2 \\
#' 0 & \text{otherwise}
#' \end{cases}
#' }
#'
#' This density is associated with the cumulative distribution function:
#' \deqn{
#' F(x) = \tfrac{3}{2}x^2 - x^3 + \tfrac{3}{16}x^4, \quad 0 \le x \le 2
#' }
#'
#' The integral of \eqn{f(x)} over \eqn{[0,2]} equals 1, ensuring it is a valid
#' probability density function.
#'
#' @return
#' A numeric vector with the density values evaluated at \code{x}.
#'
#' @examples
#' # Evaluate the density at a single point
#' df(1)
#'
#' # Evaluate the density at a sequence of points
#' x <- seq(0, 2, 0.1)
#' plot(x, df(x), type = "l", col = "red", lwd = 2,
#'      main = "Density function f(x)", ylab = "f(x)")
#'
#' @importFrom stats pchisq runif uniroot integrate
#' @export
df <- function(x) {
  ifelse(x >= 0 & x <= 2, (3/4) * x * (2 - x)^2, 0)
}


