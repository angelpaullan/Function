#' @title Chi-square Goodness-of-Fit Test
#'
#' @description
#' Performs a chi-square goodness-of-fit test to assess whether a sample
#' follows the custom distribution defined by \code{pf()}.
#'
#' @usage Chisq_f(sample)
#'
#' @param sample Numeric vector containing the observed sample.
#'
#' @details
#' The sample is divided into intervals over the support \eqn{[-1,1]}.
#' Observed frequencies are compared with the expected frequencies
#' obtained from the cumulative distribution function.
#'
#' Expected probabilities are computed as the differences between
#' consecutive values of the CDF evaluated at the interval boundaries.
#'
#' @return
#' A named numeric vector containing:
#' \itemize{
#'   \item \code{statistic}: Chi-square test statistic.
#'   \item \code{df}: Degrees of freedom.
#'   \item \code{p.value}: P-value of the test.
#' }
#'
#' @examples
#' # Generate a random sample
#' x <- randomfun(1000)
#'
#' # Apply the chi-square goodness-of-fit test
#' Chisq_f(x)
#'
#' @importFrom stats chisq.test
#' @export
Chisq_f <- function(sample){

  inter <- seq(-1, 1, by = 0.25)

  fa <- table(cut(sample,
                  breaks = inter,
                  include.lowest = TRUE))

  fx <- diff(pfun(inter))

  res <- chisq.test(as.numeric(fa), p = fx)

  c(statistic = res$statistic,
    df = res$parameter,
    p.value = res$p.value)
}
