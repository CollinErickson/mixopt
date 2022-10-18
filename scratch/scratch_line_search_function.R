index_line_search <- function(f, maxind, xarray) {
  # curve(f, 0, 300)
  # maxind <- 290
  stopifnot(maxind > 3.5)
  f2 <- function(ind) {f(xarray[ind])}
  curve(f2, 1, maxind)
  prevprevind <- 1
  prevprevy <- f2(prevprevind)
  step <- 1
  prevind <- 2
  prevy <- f2(prevind)
  nextind <- prevind + step
  nexty <- f2(nextind)
  while (nexty < prevy && nextind+.5 < maxind) {
    print(c(prevprevind, prevprevy, prevind, prevy, nextind, nexty, step))
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
    points(nextind, nexty)
  }
  # browser()
  print(c(prevprevind, prevprevy, prevind, prevy, nextind, nexty, step))

  # Switch to the 4-3 point method
  print("in 4/3 method")
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
      points(ind3, y3, col=2)
    } else {
      ind3 <- indmid
      y3 <- ymid
      ind2 <- getindbetween(ind1, ind3)
      y2 <- f2(ind2)
      points(ind2, y2, col=2)
    }
    cat(ind1, ind2, ind3, ind4, '\tys', y1, y2, y3, y4, '\n')
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
    cat("\t\t\t\t\t", ind1, indmid, ind4, '\tys', y1, ymid, y4, '\n')

  }

  list(
    ind=indmid,
    x=xarray[indmid],
    val=ymid
  )
}

if (F) {
  index_line_search(function(x) {(x-100)^2}, 290, 1:290)
}
