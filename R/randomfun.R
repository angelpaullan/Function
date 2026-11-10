#' @title Random Generation from the Custom Distribution
#'
#' @description
#' Generates random values from the custom distribution defined on the
#' interval \eqn{[-1,1]} using the inverse transform method.
#'
#' @usage randomfun(n)
#'
#' @param n Number of random observations to generate.
#'
#' @details
#' The function first generates random values from a Uniform(0,1)
#' distribution. For each generated value \eqn{u}, it numerically solves
#' the equation \eqn{F(x)=u}, where \eqn{F(x)} is the cumulative
#' distribution function of the custom distribution.
#'
#' The numerical inversion is performed using \code{uniroot()}, which
#' searches for the solution in the interval \eqn{[-1,1]}.
#'
#' The cumulative distribution function is:
#' \deqn{
#' F(x)=\frac{1}{2}+\frac{35}{32}
#' \left(x-x^3+\frac{3}{5}x^5-\frac{1}{7}x^7\right),
#' \quad -1 \le x \le 1
#' }
#'
#' @return
#' A numeric vector of length \code{n} containing random observations
#' from the custom distribution.
#'
#' @examples
#' # Generate 100 random observations
#' x <- randomfun(100)
#'
#' # Display a histogram
#' hist(x, probability = TRUE,
#'      main = "Random sample",
#'      xlab = "x")
#'
#' @importFrom stats runif uniroot
#' @export
randomfun <- function(n) {
  u <- runif(n)

  sapply(u, function(u)
    uniroot(
      function(x) 1/2 + (35/32) *
        (x - x^3 + (3/5)*x^5 - (1/7)*x^7) - u,
      interval = c(-1, 1)
    )$root
  )
}
