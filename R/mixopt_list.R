#' Index mixopt_list
#'
#' Avoid standard list indexing which returns list for single index.
#'
#' @param x x
#' @param i i
#' @param value value
#'
#' @return value at index
#' @export
#'
#' @examples
#' a <- list(1,4,'c')
#' class(a) <- "mixopt_list"
#' a[3]
#' a[2:3]
`[.mixopt_list` <- function(x, i, value) {
  # browser()
  # print("using new")
  # print(x)
  if (length(i) > 1.5) {
    # warning("[.mixopt_list doesn't work with multiple indexes")
    class(x) <- "list"
    x <- x[i]
    class(x) <- "mixopt_list"
    return(x)
  }
  x[[i]]
}

if (F) {
  b[3]
  a <- list(1,4,'c')
  a[3]
  b <- list(1,4,'c')
  class(b) <- "mixopt_list"
  b[3]
}

#' @export
print.mixopt_list <- function(x, ...) {
  cat(" [1]")
  for (i in seq_along(x)) {
    cat(" ", x[i], sep='')
  }
  cat("\n")
}


# mixopt_list_mathfunc <- function()

#' @export
sum.mixopt_list <- function(..., na.rm=FALSE) {
  dots <- list(...)
  stopifnot(length(dots) == 1)
  dot <- dots[[1]]
  s <- 0
  for (i in seq_along(dot)) {
    s <- s + dot[i]
  }
  s
}
