#' @title Spare utilities package
#'
#' @description
#'
#' This is a odds-and-ends repository for generalized functions I use in developing
#' other R packages or analyses. Sometimes other packages are required for functions
#' to work, but I dont include them in the DESCRIPTION to make installation easier.
#'
#' @author Brandon McNellis
#' @docType package
#' @name bmcUtils
NULL
#' @describeIn bmcUtils Counts days in a month
#' @family generic_utilities
#' @export
NumberOfDays <- function(date) {
  m <- format(date, format = "%m")
  while (format(date, format = "%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format = "%d")))
}
#' @describeIn bmcUtils Strips deg-min-sec vectors to 'numeric' char vector.
#' @family generic_utilities
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
#' @describeIn bmcUtils Unlists a list of 'Date' objects w/o coercion to numeric.
#' @family generic_utilities
#' @export
UnlistDate <- function(x) {
  y <- .POSIXct(list())
  class(y) <- 'Date'
  x <- lapply(x, function(x) y <<- c(y, x))
  return(y)
}
#' @describeIn bmcUtils Generates a date vector from filenames.
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
#' @describeIn bmcUtils Prints loop progress.\
#' @family generic_utilities
#' @export
LoopStatus <- function(from, to, big_inc = 1000, digits = 1) {
  if (to > big_inc) {
    if (from %% (big_inc / 10) == 0) cat('[]')
    if (from %% big_inc != 0) {
      return(invisible())
    }
  }
  prog <- from / to * 100
  prog <- round(prog, digits)
  cat(' ', prog, '%\n')
  invisible()
}
#' @describeIn bmcUtils Snips one character from each element of a character vector
#' @family generic_utilities
#' @export
SnipSingleCharacter <- function(v, side = 'front') {
  stopifnot(class(v) == 'character' && is.vector(v))
  y <- strsplit(v, '')
  if (side == 'front') {
    y <- lapply(y, function(x) x[2:(length(x))])
  } else if (side == 'back') {
    y <- lapply(y, function(x) x[1:(length(x) - 1)])
  } else {
    stop('Side must be either front or back.')
  }
  y <- lapply(y, function(x) paste(x, collapse = ''))
  return(unlist(y))
}
#' @describeIn bmcUtils Multiple ggplot panel
#' @export
Multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  # Adapted/Copied from:
  # http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
  require(grid)
  require(ggplot2)

  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots == 1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
