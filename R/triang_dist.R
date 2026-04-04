#' Density of the triangular distribution
#'
#' @param x Vector of quantiles.
#' @param min Lower limit of the distribution.
#' @param max Upper limit of the distribution.
#' @param mode Mode (peak) of the distribution.
#' @param log Logical; if TRUE, the log-density is returned.
#'
#' @return A numeric vector of density values.
#' @export
#'
#' @examples
#' dtriang(0.5, min = 0, max = 1, mode = 0.5)
#' dtriang(c(0, 0.5, 1), min = 0, max = 1, mode = 0.3)
dtriang <- function(x, min = 0, max = 1, mode = 0.5, log = FALSE) {
  if (any(min >= max)) {
    stop("'min' must be strictly less than 'max'")
  }
  if (any(mode < min | mode > max)) {
    stop("'mode' must be between 'min' and 'max'")
  }

  dens <- ifelse(
    x < min | x > max,
    0,
    ifelse(
      x <= mode,
      2 * (x - min) / ((max - min) * (mode - min + 1e-10)),
      2 * (max - x) / ((max - min) * (max - mode + 1e-10))
    )
  )

  if (log) return(base::log(dens))
  dens
}
