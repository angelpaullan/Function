#' @title Chi-square Goodness-of-Fit Test (Manual Implementation)
#'
#' @description
#' Performs a chi-square goodness-of-fit test to compare observed frequencies
#' against expected probabilities. This function manually calculates the
#' statistic, expected values, and standardized residuals, returning a
#' standard \code{htest} object.
#'
#' @param fa Numeric vector of observed frequencies.
#' @param probs Numeric vector of expected probabilities (must sum to 1).
#'
#' @return An object of class \code{htest} containing:
#' \itemize{
#'   \item statistic : The calculated Chi-square statistic.
#'   \item parameter : Degrees of freedom (\eqn{k - 1}).
#'   \item p.value   : The p-value of the test.
#'   \item method    : Character string indicating the type of test.
#'   \item observed  : The provided observed frequencies.
#'   \item expected  : The calculated expected frequencies (\eqn{n \times p_i}).
#'   \item residuals : Pearson residuals.
#'   \item stdres    : Standardized residuals.
#' }
#'
#' @details
#' The function follows the standard Chi-square test logic. A warning is issued
#' if any expected frequency is less than 5, as the Chi-square approximation
#' may not be reliable in such cases.
#'
#' @examples
#' # Example: Testing a fair six-sided die
#' obs <- c(10, 12, 9, 11, 8, 10)
#' p_theoretical <- rep(1/6, 6)
#' pr_bond_ajus(obs, p_theoretical)
#'
#' @export
pr_bond_ajus <- function(fa, probs) {
  n <- sum(fa)
  E <- n * probs
  V <- n * probs * (1 - probs)

  STATISTIC <- sum((fa - E)^2 / E)
  PARAMETER <- length(fa) - 1
  PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)

  structure(list(
    statistic = structure(STATISTIC, names = "X-squared"),
    parameter = structure(PARAMETER, names = "df"),
    p.value = PVAL,
    method = "Chi-squared test for given probabilities",
    data.name = deparse(substitute(fa)),
    observed = fa,
    expected = E,
    residuals = (fa - E)/sqrt(E),
    stdres = (fa - E)/sqrt(V)
  ), class = "htest")
}
