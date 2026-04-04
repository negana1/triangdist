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


#' Distribution function of the triangular distribution
#'
#' @param q Vector of quantiles.
#' @param min Lower limit of the distribution.
#' @param max Upper limit of the distribution.
#' @param mode Mode (peak) of the distribution.
#' @param lower.tail Logical; if TRUE (default), probabilities are P(X <= q).
#'
#' @return A numeric vector of probabilities.
#' @export
#'
#' @examples
#' ptriang(0.5, min = 0, max = 1, mode = 0.5)
#' ptriang(c(0, 0.5, 1), min = 0, max = 1, mode = 0.3)
ptriang <- function(q, min = 0, max = 1, mode = 0.5, lower.tail = TRUE) {
  if (any(min >= max)) {
    stop("'min' must be strictly less than 'max'")
  }
  if (any(mode < min | mode > max)) {
    stop("'mode' must be between 'min' and 'max'")
  }

  prob <- ifelse(
    q <= min,
    0,
    ifelse(
      q >= max,
      1,
      ifelse(
        q <= mode,
        (q - min)^2 / ((max - min) * (mode - min + 1e-10)),
        1 - (max - q)^2 / ((max - min) * (max - mode + 1e-10))
      )
    )
  )

  if (!lower.tail) return(1 - prob)
  prob
}


#' Quantile function of the triangular distribution
#'
#' @param p Vector of probabilities. Must be in [0, 1].
#' @param min Lower limit of the distribution.
#' @param max Upper limit of the distribution.
#' @param mode Mode (peak) of the distribution.
#'
#' @return A numeric vector of quantiles.
#' @export
#'
#' @examples
#' qtriang(0.5, min = 0, max = 1, mode = 0.5)
#' qtriang(c(0, 0.25, 0.5, 0.75, 1), min = 0, max = 1, mode = 0.3)
qtriang <- function(p, min = 0, max = 1, mode = 0.5) {
  if (any(min >= max)) {
    stop("'min' must be strictly less than 'max'")
  }
  if (any(mode < min | mode > max)) {
    stop("'mode' must be between 'min' and 'max'")
  }
  if (any(p < 0 | p > 1, na.rm = TRUE)) {
    stop("'p' must be in [0, 1]")
  }

  p_c <- (mode - min) / (max - min)

  ifelse(
    p <= p_c,
    min + sqrt(p * (max - min) * (mode - min + 1e-10)),
    max - sqrt((1 - p) * (max - min) * (max - mode + 1e-10))
  )
}


#' Random generation for the triangular distribution
#'
#' @param n Number of random values to generate.
#' @param min Lower limit of the distribution.
#' @param max Upper limit of the distribution.
#' @param mode Mode (peak) of the distribution.
#'
#' @return A numeric vector of n random values.
#' @export
#'
#' @examples
#' set.seed(42)
#' rtriang(10, min = 0, max = 1, mode = 0.5)
rtriang <- function(n, min = 0, max = 1, mode = 0.5) {
  if (any(min >= max)) {
    stop("'min' must be strictly less than 'max'")
  }
  if (any(mode < min | mode > max)) {
    stop("'mode' must be between 'min' and 'max'")
  }
  if (length(n) != 1 || !is.numeric(n) || n < 0 || n != floor(n)) {
    stop("'n' must be a non-negative integer")
  }
  qtriang(stats::runif(n), min = min, max = max, mode = mode)
}

