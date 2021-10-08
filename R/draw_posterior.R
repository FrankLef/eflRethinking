#' Posterior samples using \code{rethinking::extract.samples} as
#' \code{draws_rvars}
#'
#' Posterior samples using \code{rethinking::extract.samples} as
#' \code{draws_rvars}.
#'
#' get posterior samples from a \code{map} object formatted as a \code{draws_rvars}
#' object to be used by the \code{posterior} package.
#'
#' @param obj S4 object of class \code{map}.
#' @param n Sample size.
#'
#' @importFrom MASS mvrnorm
#'
#' @return \code{draws_rvars} object
#' @export
draw_posterior_quap <- function(obj, n = 1L) {
  checkmate::assert_class(obj, class = "map", ordered = TRUE)
  checkmate::assert_count(n, positive = TRUE)

  out <- rethinking::extract.samples(obj, n = n)
  # out <- MASS::mvrnorm(n = n, mu = obj@coef, Sigma = obj@vcov)
  # cat("\n", "inside draw_posterior_map", "\n")
  # print(out)
  # cat("\n")

  posterior::as_draws_rvars(out)
}
