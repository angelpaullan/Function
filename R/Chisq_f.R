#' @title Chi-square Goodness-of-Fit Test for a sample of the Pf Distribution
#'
#' @description
#' Performs a chi-square goodness-of-fit test comparing observed frequencies
#' from a sample with expected probabilities derived from the distribution:
#'
#' \deqn{
#' f(x) =
#' \begin{cases}
#' \tfrac{3}{4} \, x \, (2 - x)^2, & 0 \le x \le 2 \\
#' 0, & \text{otherwise}
#' \end{cases}
#' }
#'
#' The interval [0, 2] is divided into bins of width 0.25.
#'
#' @param sample Numeric vector of observations.
#'
#' @return A named numeric vector with:
#' \itemize{
#'   \item statistic : Chi-square statistic
#'   \item df        : Degrees of freedom
#'   \item p.val     : p-value
#' }
#'
#' @details
#' This function tests the hypotheses \eqn{H_0: \pi = \hat{\pi}} vs. \eqn{H_1: \pi \neq \hat{\pi}}.
#' Expected probabilities are computed as differences of the CDF \eqn{F(x)} over each interval.
#' Observed frequencies in the sample are obtained using the \code{fdth} package.
#'
#' @examples
#' sample <- randomfun2(1000)
#' Chisq_f(sample)
#'
#' @importFrom fdth fdt
#' @importFrom stats chisq.test
#'
#' @export
Chisq_f <- function(sample) {
  # Definir cortes en el soporte [0,2]
  x <- seq(0, 2, by = 0.25)

  # Probabilidades teóricas en cada intervalo usando tu CDF pf()
  fx <- diff(pf(x))

  # Frecuencias observadas con tabla de frecuencias
  t <- fdt(sample, start = 0, end = 2, h = 0.25)
  fa <- t$table$f

  # Prueba chi-cuadrado
  res <- chisq.test(x = fa, p = fx)

  # Resultado resumido
  c("statistic" = as.numeric(res$statistic),
    "df"        = as.numeric(res$parameter),
    "p.val"     = res$p.value)
}
