full_line_search <- function(f, xarray, startind) {
  # startind <- 10
  # xarray <- sort(runif(1e2, 0, 100))
  maxind <- length(xarray)
  # f <- function(x) {cos(x/15)}
  plot(xarray, f(xarray))
  f2 <- function(ind) {f(xarray[ind])}

  # If <= 10 inds, just eval all
  if (maxind <= 10) {
    y <- rep(NA, maxind)
    for (i in 1:maxind) {
      y[i] <- f(xarray[i])
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
  } else if (startind == maxind) {
    direction <- "L"
  } else {
    # Both directions are valid, so check both
    ystart <- f2(startind)
    yleft1 <- f2(startind-1)
    yright1 <- f2(startind+1)
    if (ystart <= yleft1 && ystart <= yright1) {
      # No improvement in either direction
      return(list(ind=startind,
                  x=xarray[startind],
                  val=y[startind]))
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

  # If left is lower, go left
  index_line_search(f=f, xarray=xarray[1:(startind)])

  # If right is lower, go right


}
