#' @title Random pseudo-numbers generator via Newton-Raphson
#'
#' @description
#' Generates pseudo-random numbers from a continuous distribution defined by
#' a quartic cumulative distribution function (CDF), using the inverse transform
#' method resovled by Newton-Raphson algorithm.
#'
#' @param n Integer. Length of the pseudo-random vector to be generated.
#' @param tol Numeric. Tolerance for convergence (default 1e-8).
#' @param max_iter Integer. Maximum number of iterations (default 100).
#'
#' @details
#' This function applies the inverse transform method:
#' \deqn{F(x) = \tfrac{3}{2}x^2 - x^3 + \tfrac{3}{16}x^4, \quad x \in [0,2]}
#'
#' The equation \eqn{F(x) - u = 0} is solved using the Newton-Raphson
#' iterative scheme:
#' \deqn{x_{n+1} = x_n - \frac{F(x_n) - u}{f(x_n)}}
#'
#' @return
#' A numeric vector of length \code{n} with random values.
#'
#' @examples
#' # Generate 100 random values
#randomfun2(100)
#'
#' @export
randomfun2 <- function(n, tol = 1e-8, max_iter = 100) {

  # F(x) - u
  cdf_u <- function(x, u) { (3/2)*x^2 - x^3 + (3/16)*x^4 - u }

  # f(x) (derivada de la CDF)
  pdf <- function(x) { 3*x - 3*x^2 + (3/4)*x^3 }

  u_val <- runif(n)

  x_gen <- sapply(u_val, function(u) {
    x_curr <- 1 # Punto inicial
    for(j in 1:max_iter) {
      f_val <- cdf_u(x_curr, u)
      f_prime <- pdf(x_curr)

      if (abs(f_prime) < 1e-10) break

      x_next <- x_curr - f_val / f_prime

      # Restricción al soporte [0,2]
      if (x_next < 0) x_next <- 0
      if (x_next > 2) x_next <- 2

      if (abs(x_next - x_curr) < tol) return(x_next)
      x_curr <- x_next
    }
    return(x_curr)
  })

  return(x_gen)
}
