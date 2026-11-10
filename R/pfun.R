#' Función de distribución acumulada de la densidad
#' f(x) = (35/32)(1-x^2)^3, para -1 <= x <= 1.
#'
#' Calcula la probabilidad acumulada F(x)=P(X<=x)
#' asociada a la función de densidad
#'
#' \deqn{
#' f(x)=\frac{35}{32}(1-x^2)^3,\quad -1\le x\le 1.
#' }
#'
#' La expresión cerrada de la distribución acumulada es
#'
#' \deqn{
#' F(x)=
#' \begin{cases}
#' 0, & x < -1,\\
#' \frac{1}{2}+\frac{35}{32}
#' \left(
#' x-x^3+\frac{3}{5}x^5-\frac{1}{7}x^7
#' \right), & -1\le x\le 1,\\
#' 1, & x > 1.
#' \end{cases}
#' }
#'
#' @param x Vector numérico de valores donde se evalúa la función
#'   de distribución acumulada.
#'
#' @return Un vector numérico con las probabilidades acumuladas
#'   correspondientes a cada valor de \code{x}.
#'
#' @details
#' La distribución es simétrica respecto a cero y está soportada
#' únicamente en el intervalo \eqn{[-1,1]}.
#'
#' @examples
#' pfun(0)
#' pfun(c(-1, 0, 1))
#' pfun(seq(-1, 1, length.out = 5))
#'
#' @export
pfun <- function(x) {
  ifelse(x < -1, 0,
         ifelse(x <= 1,
                1/2 + (35/32) *
                  (x - x^3 + (3/5) * x^5 - (1/7) * x^7),
                1))
}
