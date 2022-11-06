#' Optimize over array using line search
#'
#' @param f Function
#' @param xarray Array of values
#' @param startind Starting index
#' @param plot Should plots be made?
#' @param verbose Level of info to print
#' @param ystart Value of f when evaluated on element of xarray at index
#' startind, aka f(xarray[startind])
#'
#' @return List
#' @export
#'
#' @examples
#' full_index_line_search(function(x) {(x-50)^2}, 3:12, 5)
#' full_index_line_search(function(x) {(x-50)^2}, 3, 1)
#' full_index_line_search(function(x) {(x-50)^2}, 3:4, 1)
#' full_index_line_search(function(x) {(x-50)^2}, 3:5, 1)
#' full_index_line_search(function(x) {(x+50)^2}, 3, 1)
#' full_index_line_search(function(x) {(x+50)^2}, 3:4, 1)
#' full_index_line_search(function(x) {(x+50)^2}, 3:5, 1)
#' full_index_line_search(function(x) {(x-50)^2}, 12:3, 8)
#' full_index_line_search(function(x) {(x-50)^2}, 0:1000, 8)
#' full_index_line_search(function(x) {(x-50)^2}, 0:1000, 999)
#' full_index_line_search(function(x) {sin(x/30)}, 0:1000, 999)
full_index_line_search <- function(f, xarray, startind, plot="none",
                                   ystart=NULL,
                                   verbose=0) {
  if (verbose >= 10) {
    cat("Entering full_index_line_search", "\n")
  }
  # startind <- 10
  # xarray <- sort(runif(1e2, 0, 100))
  stopifnot(plot %in% c("none", "ind", "x"))
  maxind <- length(xarray)
  stopifnot(is.numeric(startind), startind >= 1, startind <= maxind,
            abs(startind-round(startind))<.001)
  # f <- function(x) {cos(x/15)}
  f2 <- function(ind) {f(xarray[ind])}

  if (is.null(ystart)) {
    ystart <- f2(startind)
  }
  stopifnot(length(ystart) == 1)

  if (plot == "ind") {
    plot(xarray, f2(1:maxind))
  } else if (plot == "x") {
    plot(xarray, f(xarray))
  }

  # If few inds, just eval all
  if (maxind <= 3) {
    if (verbose >= 10) {
      cat("  In index_line_search, few values, just checking all", "\n")
    }
    y <- rep(NA, maxind)
    for (i in 1:maxind) {
      if (i == startind && !is.null(ystart)) {
        y[i] <- ystart
      } else {
        y[i] <- f(xarray[i])
      }
    }
    minind <- which.min(y)
    return(
      list(
        ind=minind,
        x=xarray[minind],
        val=y[minind]
      )
    )
  }

  if (startind == 1) {
    direction <- "R"
    yright1 <- f2(startind+1)
  } else if (startind == maxind) {
    direction <- "L"
    yleft1 <- f2(startind-1)
  } else {
    # Both directions are valid, so check both
    # ystart <- f2(startind)
    yleft1 <- f2(startind-1)
    yright1 <- f2(startind+1)
    if (ystart <= yleft1 && ystart <= yright1) {
      # No improvement in either direction
      return(list(ind=startind,
                  x=xarray[startind],
                  val=ystart))
    } else if (yleft1 <= yright1) {
      direction <- "L"
    } else if (yright1 <= yleft1) {
      direction <- "R"
    } else {
      stop("Error 847128")
    }
  }

  # If ind>=2, check left 1 index

  # If ind<=n-1, check right 1 index

  # If neither lower, return start

  if (direction == "L") {
    # If left is lower, go left
    out <- index_line_search(f=f, xarray=rev(xarray[1:(startind)]),
                             plot=plot, verbose=verbose,
                             y1=yleft1)
    out_ind_corrected <- startind - out$ind + 1
    return(list(ind=out_ind_corrected,
                x=xarray[out_ind_corrected],
                val=out$val
    ))
  } else if (direction == "R") {
    # If right is lower, go right
    out <- index_line_search(f=f, xarray=xarray[(startind+1):maxind],
                             plot=plot, verbose=verbose,
                             y1=yright1)
    out_ind_corrected <- startind + out$ind
    return(list(ind=out_ind_corrected,
                x=xarray[out_ind_corrected],
                val=out$val
    ))
  } else {
    stop("Error 1898715")
  }
  stop("Error 9148100")


}

if (F) {
  full_index_line_search(function(x) {(x-50)^2}, 3:12, 5)
  full_index_line_search(function(x) {(x-50)^2}, 3, 1)
  full_index_line_search(function(x) {(x-50)^2}, 3:4, 1)
  full_index_line_search(function(x) {(x-50)^2}, 3:5, 1)
  full_index_line_search(function(x) {(x+50)^2}, 3, 1)
  full_index_line_search(function(x) {(x+50)^2}, 3:4, 1)
  full_index_line_search(function(x) {(x+50)^2}, 3:5, 1)
  full_index_line_search(function(x) {(x-50)^2}, 12:3, 8)
  full_index_line_search(function(x) {(x-50)^2}, 0:1000, 8)
  full_index_line_search(function(x) {(x-50)^2}, 0:1000, 999)
  full_index_line_search(function(x) {sin(x/30)}, 0:1000, 999)
}
