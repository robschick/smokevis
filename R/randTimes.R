#' Return a Vector of N Times Within a Day
#'
#' The goal of this is to return a random sample of times
#' between two known end points.
#' This comes from a stack overflow post: http://goo.gl/LDHqSF;
#' the answer and code comes from Dirk Edelbuettel, but I've modified
#' it slightly to deal with posix times input
#' @param N - number of events to return
#' @param st - starting time in POSIX form: YYYY-MM-DD HH:MM:SS
#' @param st - ending time in POSIX form: YYYY-MM-DD HH:MM:SS
#' @export
#' @examples
#' randTimes(5)
randTimes <- function(N, st = "2016-06-14 07:30:00", et = "2016-06-14 22:30:00") {
  st <- as.POSIXct(st)
  et <- as.POSIXct(et)
  dt <- as.numeric(difftime(et, st, unit = "sec"))
  ev <- sort(runif(N, 0, dt))
  rt <- st + ev
  rt
}
