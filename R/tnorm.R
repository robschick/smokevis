#' Truncated Normal Distribution
#'
#' Random generation for a truncated normal distribution for \code{n}
#' random deviates, with lower bound equal to \code{lo}, upper bound equal
#' to \code{hi}, mean equal \code{mu}, standard deviation equal to \code{sd}.
#'
#' @source Code written by Jim Clark, Duke University, Durham, NC, USA
#'
#' @param n - integer specifying the number of draws from the distribition to
#'   return
#' @param lo - the lower bound specified for the distribution
#' @param hi - the upper bound specified for the distribution
#' @param mu - the mean of the distribution
#' @param sig - the standard deviation of the distribution
#' @export
#' @examples
#' tnorm(5, 2, 7, 5, 1)
#' ceiling(tnorm(5, 2, 7, 5, 1) / 2) / 2
tnorm <- function(n, lo, hi, mu, sig){

  if(length(lo) == 1 & length(mu) > 1)lo <- rep(lo, length(mu))
  if(length(hi) == 1 & length(mu) > 1)hi <- rep(hi, length(mu))

  q1 <- pnorm(lo, mu, sig)
  q2 <- pnorm(hi, mu, sig)

  z <- runif(n, q1, q2)
  z <- qnorm(z, mu, sig)
  z[z == Inf]  <- lo[z == Inf]
  z[z == -Inf] <- hi[z == -Inf]
  z
}
