#' @title Internal utility functions.
#'
#' @description
#'
#' `NumberOfDays` counts the days in a month. Input is an object of class
#' `date`, and it will work for leap years etc.
#'
#' `StripDegMinSec` will convert a lat/long degree-min-sec character vector
#' to a 'numeric' character vector, where each character is three numeric
#' values seperated by single spaces. This makes the vector usable by
#' the `conv_units` function in the `measurements` package >= v. 1.2.0.
#'
#' `UnlistDate` will unlist a list of 'Date' objects without coercing them to
#' numeric.
#'
#' @name utils
#' @export
#' @examples NumberOfDays(Sys.Date())
utils <- function() {
  message('Available functions:')
  avail <- c(
    'NumberOfDays', 'StrpDegMinSec', 'UnlistDate'
  )
  return(avail)
}
#' @describeIn utils Counts days in a month
NumberOfDays <- function(date) {
  m <- format(date, format = "%m")
  while (format(date, format = "%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format = "%d")))
}
#' @describeIn utils Strips deg-min-sec vectors to 'numeric' char vector.
StripDegMinSec <- function(x) {
  x <- regmatches(x, gregexpr("[[:digit:]]+", x))
  x <- lapply(x, function(z) paste(z, collapse = ' '))
  x <- unlist(lapply(x, function(z) strsplit(z, ' ')), recursive = F)
  l <- lapply(x, length)
  ret <- vector(mode = 'numeric', length = length(l))
  for (i in 1:length(l)) {
    if (l[i] == 3) {
      ret[i] <- paste(unlist(x[i]), collapse = ' ')
    }
    if (l[i] == 4) {
      y <- unlist(x[i])
      w <- paste(y[3], y[4], sep = '.')
      ret[i] <- paste(y[1], y[2], w, collapse = ' ')
    }
  }
  return(ret)
}
#' @describeIn utils Unlists a list of 'Date' objects w/o coercion to numeric.
UnlistDate <- function(x) {
  y <- .POSIXct(list())
  class(y) <- 'Date'
  x <- lapply(x, function(x) y <<- c(y, x))
  return(y)
}
