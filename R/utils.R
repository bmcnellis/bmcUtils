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
#' `PullFilenameDates` extracts dates of the given format from the current
#' working directory. Assumes that the format doesn't include alphabetic
#' characters.
#'
#' `clear` is a shortcut function that (1) clears the environment, (2) runs GC,
#' (3) clears the console, and (4) restarts R.
#'
#' @name utils
#' @export
#' @examples NumberOfDays(Sys.Date())
utils <- function() {
  message('Available functions:')
  avail <- c(
    'NumberOfDays', 'StrpDegMinSec', 'UnlistDate', 'clear'
  )
  return(avail)
}
#' @describeIn utils Counts days in a month
#' @export
NumberOfDays <- function(date) {
  m <- format(date, format = "%m")
  while (format(date, format = "%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format = "%d")))
}
#' @describeIn utils Strips deg-min-sec vectors to 'numeric' char vector.
#' @export
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
#' @export
UnlistDate <- function(x) {
  y <- .POSIXct(list())
  class(y) <- 'Date'
  x <- lapply(x, function(x) y <<- c(y, x))
  return(y)
}
#' @describeIn utils Generates a date vector from filenames.
#' @export
PullFilenameDates <- function(fmt) {
  if (nchar(fmt) < 6) stop('Bad fmt input.')
  j_funct <- function(x, n, fmt) {
    for (j in 1:(length(x) - n)) {
      sub <- as.Date(paste(x[seq.int(j, j + n - 1)], collapse = ''), fmt)
      if (!is.na(sub)) {
        return(as.character(sub))
      }
    }
    return(NA)
  }
  #y <- gsub('[A-Za-z]', '', list.files())
  y <- list.files()
  n <- nchar(format(as.Date(Sys.Date()), format = fmt))
  out <- vector('character', length(y))
  for (i in 1:length(y)) {
    x <- unlist(strsplit(y[i], ''))
    out[i] <- j_funct(x = x, n = n, fmt = fmt)
  }
  return(out)
}
#' @describeIn utils Clears multiple aspects of the R environment.
#' @export
clear <- function() {
  suppressWarnings(file.remove('.Rhistory'))
  suppressWarnings(file.remove('.Rdata'))
  invisible(gc())
  cat("\014")
  save.image()
  on.exit(rm(list = ls()))
  on.exit(invisible(.rs.restartR()), add = T)
  invisible()
}
