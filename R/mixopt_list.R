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
#' a <- list(1,4,'c', 'g')
#' class(a) <- "mixopt_list"
#' a
#' a[3]
#' a[2:3]
#' a[-(2:3)]
#' as.data.frame(a)
#'
#' b <- as.mixopt_list(c(1,2,3,4,5))
#' sum(b)
#' b^2
#' b+b
#' b-b
#' b*b
#' b/b
#' c(b)
#' c(b, b)
#' c(b, 1)
#' c(1, b)
#' c(a, b, a)
#' c_mixopt_list(0, 1, 2, 3, 4, a, 5, 6, 7, 8, b, 9)
#' c_mixopt_list(NULL, 3, NULL, a, NULL, 66666, NULL, b)
`[.mixopt_list` <- function(x, i, value) {
  if (length(i) > 1.5) {
    # warning("[.mixopt_list doesn't work with multiple indexes")
    class(x) <- "list"
    x <- x[i]
    # class(x) <- "mixopt_list"
    # This will simplify to numeric/char if possible, allowing math operators
    x <- as.mixopt_list(x, T)
    return(x)
  }
  if (any(i < 0)) {
    if (any(i > 0)) {
      stop(
        "Can't mix positive and negative indexes when subsetting mixopt_list")
    }
    i <- i[i<0]
    actual_i <- setdiff(1:length(x), as.integer(-i))
    return(x[actual_i])
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
  # browser()
  cat("mixopt_list: [1]")
  for (i in seq_along(x)) {
    cat(" ", x[i], sep='')
  }
  cat("\n")
}

#' Checks if object is mixopt_list
#'
#' @param x Object
#' @return TRUE if x has class "mixopt_list"
#'
#' @export
is.mixopt_list <- function(x) {
  "mixopt_list" %in% class(x)
}

#' Coerce to a mixopt_list
#'
#' @param x Object
#' @param simplifyifpossible If possible, should the class be simplified to
#' numeric or character?
#' @return Object of class mixopt_list
#'
#' @export
as.mixopt_list <- function(x, simplifyifpossible=FALSE) {
  if (simplifyifpossible && (is.numeric(x) || is.character(x))) {
    return(x)
  }
  if (simplifyifpossible && is.list(x)) {
    if (all(sapply(x, is.numeric))) {
      return(unlist(x))
    }
    if (all(sapply(x, is.character))) {
      return(unlist(x))
    }
  }
  if ("mixopt_list" %in% class(x)) {
    # Do nothing
  } else {
    x <- as.list(x)
    stopifnot(all(sapply(x, length) == 1))
    class(x) <- "mixopt_list"
  }
  x
}

#' Combines mixopt_list objects
#'
#' @param x Object
#'
#' @param ... Additional objects
#' @return A combined mixopt_list
#'
#' @export
#' @examples
#' c_mixopt_list(NULL, as.mixopt_list(1:5), NULL, as.mixopt_list(letters[1:5]))
#' c_mixopt_list(as.mixopt_list(1:3), NULL)
c_mixopt_list <- function(x, ...) {
  # browser()
  if (is.null(x)) { #print('hasnull')
    dots <- list(...)
    if (length(dots) == 0) {
      return(NULL)
    }
    return(do.call(c_mixopt_list, dots))
    # return(c_mixopt_list(...))
  }
  if (!is.mixopt_list(x)) {
    # browser()
    x <- as.list(x)
    class(x) <- "mixopt_list"
    # do.call(c, x, ...)
  }

  dots <- list(...)
  if (length(dots) > .5) {
    c(x, ...)
  } else {
    c(x)
  }
}

#' @export
c.mixopt_list <- function(x, ...) {
  dots <- list(...)
  if (length(dots) < .5) {
    return(x)
  }
  if (length(dots) > 1.5) {
    for (i in 1:length(dots)) {
      x <- c(x, dots[[i]])
    }
    return(x)
    # t1 <- c(x, dots[[1]])
    # dotsleft <- dots[2:]
    # if (length(dotsleft) == 1) {
    #   return(c(t1, dotsleft[[1]]))
    # }
    # return(do.call(c(t1, dots))
  }
  a <- x
  b <- list(...)[[1]]
  # browser()
  if (is.mixopt_list(a)) {
    class(a) <- "list"
  } else {
    a <- as.list(a)
  }
  if (is.mixopt_list(b)) {
    class(b) <- "list"
  } else {
    b <- as.list(b)
  }
  out <- c(a, b)
  class(out) <- "mixopt_list"
  out
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


#' @export
`^.mixopt_list` <- function(e1, e2) {
  for (i in 1:length(e1)) {
    e1[i] <- e1[i]^e2
  }
  e1
}

#' @export
`+.mixopt_list` <- function(e1, e2) {
  out <- numeric(max(length(e1), length(e2)))
  for (i in 1:max(length(e1), length(e2))) {
    out[i] <- e1[min(i, length(e1))] + e2[min(i, length(e2))]
  }
  out
}

#' @export
`-.mixopt_list` <- function(e1, e2) {
  out <- numeric(max(length(e1), length(e2)))
  for (i in 1:max(length(e1), length(e2))) {
    out[i] <- e1[min(i, length(e1))] - e2[min(i, length(e2))]
  }
  out
}

#' @export
`*.mixopt_list` <- function(e1, e2) {
  out <- numeric(max(length(e1), length(e2)))
  for (i in 1:max(length(e1), length(e2))) {
    out[i] <- e1[min(i, length(e1))] * e2[min(i, length(e2))]
  }
  out
}

#' @export
`/.mixopt_list` <- function(e1, e2) {
  out <- numeric(max(length(e1), length(e2)))
  for (i in 1:max(length(e1), length(e2))) {
    out[i] <- e1[min(i, length(e1))] / e2[min(i, length(e2))]
  }
  out
}

#' @export
as.data.frame.mixopt_list <- function(x, row.names=NULL, optional=FALSE, ...) {
  x2 <- as.data.frame.list(x)
  colnames(x2) <- paste0("X", 1:ncol(x2))
  x2
}
