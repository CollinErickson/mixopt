#' Line search over indexed array in one direction
#'
#' @param f f
#' @param xarray xarray
#' @param y1 y1
#' @param plot plot
#' @param verbose Level to print
#'
#' @return List
#' @export
#' @importFrom graphics curve points
#'
#' @examples
#' index_line_search(function(x) {(x-100)^2}, 1:290)
#' index_line_search(function(x) {(-x-100)^2}, -(1:290)^.92, plot="ind")
#' index_line_search(function(x) {(-x-100)^2}, -(1:290)^.92, plot="x")
#' xx <- sort(runif(1e2, -250, -30))
#' index_line_search(function(x) {(-x-100)^2}, xx, plot="ind")
#' index_line_search(function(x) {(-x-100)^2}, xx, plot="x")
index_line_search <- function(f, xarray,
                              y1=NULL,
                              plot="none",
                              verbose=0) {
  if (verbose >= 10) {
    cat("Entering index_line_search", "\n")
  }
  # curve(f, 0, 300)
  # maxind <- 290
  maxind <- length(xarray)
  # stopifnot(maxind)
  stopifnot(plot %in% c("none", "ind", "x"))
  f2 <- function(ind) {
    f(xarray[ind])
  }

  if (is.null(y1)) {
    # y1 <- f2(xarray[1])
    y1 <- f2(1)
  }

  # If maxind is small, try all values
  if (maxind == 1) {
    return(list(
      ind=1,
      x=xarray[1],
      val=y1
    ))
  }
  y2 <- f2(2)
  if (maxind == 2) {
    if (y1 <= y2) {
      return(list(
        ind=1,
        x=xarray[1],
        val=y1
      ))
    } else {
      return(list(
        ind=2,
        x=xarray[2],
        val=y2
      ))
    }
  }

  # If not a decreasing direction, return starting point
  if (y1 < y2) {
    return(list(
      ind=1,
      x=xarray[1],
      val=y1
    ))
  }

  if (maxind == 3) {
    y3 <- f2(3)
    ind <- which.min(c(y1, y2, y3))[1]
    return(list(
      ind=ind,
      x=xarray[ind],
      val=min(c(y1, y2, y3))
    ))
  }

  stopifnot(maxind > 3.5)

  if (plot == "ind") {
    curvfunc <- Vectorize(function(x) {f2(round(x))})
    curve(curvfunc, 1, maxind)
  } else if (plot == "x") {
    curve(f, xarray[1], xarray[maxind])
  }
  # pointsfunc <- function
  prevprevind <- 1
  prevprevy <- f2(prevprevind)
  step <- 1
  prevind <- 2
  prevy <- f2(prevind)
  nextind <- prevind + step
  nexty <- f2(nextind)
  while (nexty < prevy && nextind+.5 < maxind) {
    if (verbose >= 12) {
      print(c(prevprevind, prevprevy, prevind, prevy, nextind, nexty, step))
    }
    prevprevind <- prevind
    prevprevy <- prevy
    prevind <- nextind
    prevy <- nexty
    step <- step * 2
    nextind <- prevind + step
    if (nextind > maxind+.5) {
      nextind <- maxind
    }
    nexty <- f2(nextind)
    if (plot == "ind") {
      points(nextind, nexty)
    } else if (plot == "x") {
      points(xarray[nextind], nexty)
    }
  }
  if (verbose >= 7) {
    print(c(prevprevind, prevprevy, prevind, prevy, nextind, nexty, step))
  }

  # TODO: if nextind == maxind,

  # Switch to the 4-3 point method
  if (verbose >= 7) {
    print("in 4/3 method")
  }
  getindbetween <- function(i1, i2) {
    if (i1+1.5 >= i2) {
      stop("No ind between")
    }
    round(.5*(i1+i2))
  }
  # Convert last step to 1/2/3
  ind1 <- prevprevind
  y1 <- prevprevy
  indmid <- prevind
  ymid <- prevy
  ind4 <- nextind
  y4 <- nexty

  # Loop until done
  while (ind4 - indmid > 1.5 || indmid - ind1 > 1.5) {
    # if (ind1==81) {stop('81aaaa')}
    # Convert 3 to 4
    if (ind4 - indmid >= indmid - ind1) {
      ind2 <- indmid
      y2 <- ymid
      ind3 <- getindbetween(ind2, ind4)
      y3 <- f2(ind3)
      # points(ind3, y3, col=2)
      if (plot == "ind") {
        points(ind3, y3, col=2)
      } else if (plot == "x") {
        points(xarray[ind3], y3, col=2)
      }
    } else {
      ind3 <- indmid
      y3 <- ymid
      ind2 <- getindbetween(ind1, ind3)
      y2 <- f2(ind2)
      # points(ind2, y2, col=2)
      if (plot == "ind") {
        points(ind2, y2)
      } else if (plot == "x") {
        points(xarray[ind2], y2)
      }
    }
    if (verbose >= 7) {
      cat(ind1, ind2, ind3, ind4, '\tys', y1, y2, y3, y4, '\n')
    }
    # Convert 4 to 3
    if (y2 > y3) {
      ind1 <- ind2
      y1 <- y2
      indmid <- ind3
      ymid <- y3
    } else {
      ind4 <- ind3
      y4 <- y3
      indmid <- ind2
      ymid <- y2
    }
    if (verbose >= 7) {
      cat("\t\t\t\t\t", ind1, indmid, ind4, '\tys', y1, ymid, y4, '\n')
    }

  }

  list(
    ind=indmid,
    x=xarray[indmid],
    val=ymid
  )
}

if (F) {
  index_line_search(function(x) {(x-100)^2}, 1:290)
  index_line_search(function(x) {(-x-100)^2}, -(1:290)^.92, plot="ind")
  index_line_search(function(x) {(-x-100)^2}, -(1:290)^.92, plot="x")
  xx <- sort(runif(1e2, -250, -30))
  index_line_search(function(x) {(-x-100)^2}, xx, plot="ind")
  index_line_search(function(x) {(-x-100)^2}, xx, plot="x")
}
