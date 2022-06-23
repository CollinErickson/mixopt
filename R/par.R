
#' Verify parameters
#'
#' @param par List of parameters
#'
#' @return Nothing, raises error if not valid
#' @export
#'
#' @examples
#' verify_par(
#'   list(
#'     mopar_cts(2, 8, 6)
#'   )
#' )
verify_par <- function(par) {
  stopifnot(is.list(par))
  stopifnot(length(par) > .5)
  for (i in 1:length(par)) {
    if (!(("mixopt_par" %in% class(par[[i]])))) {
      browser()
    }
    stopifnot("mixopt_par" %in% class(par[[i]]))
  }
  # start_par <- list()
  # for (i in 1:length(par)) {
  #   stopifnot(is.character(par[[i]][[1]]))
  #   stopifnot(length(par[[i]][[1]]) == 1)
  #   if (par[[i]][[1]] %in% c("cts")) {
  #     stopifnot(is.numeric(par[[i]][[2]]))
  #     stopifnot(length(par[[i]][[2]]) == 2)
  #     stopifnot(par[[i]][[2]][1] <= par[[i]][[2]][2])
  #     # stopifnot(par[[i]][[2]])
  #     if (length(par[[i]]) > 2.5) {
  #       stopifnot(is.numeric(par[[i]][[3]]))
  #       stopifnot(length(par[[i]][[1]]) == 1)
  #       stopifnot(par[[i]][[2]][[1]] <= par[[i]][[3]])
  #       stopifnot(par[[i]][[2]][[2]] >= par[[i]][[3]])
  #       start_par[[i]] <- par[[i]][[3]]
  #     } else {
  #       start_par[[i]] <- mean(par[[i]][[2]])
  #     }
  #   } else {
  #     stop(paste0("par ", i, "doesn't have proper type"))
  #   }
  # }
  # return(start_par)
}

#' Continuous variable
#'
#' @param lower Lower
#' @param upper Upper
#' @param start Start. Defaults to midpoint if not given.
#'
#' @importFrom stats runif
#' @return mixopt_par list
#' @export
#'
#' @examples
#' mopar_cts(2,8)
#' mopar_cts(2,8,7)
mopar_cts <- function(lower, upper, start=NULL) {
  # Verify valid bounds
  stopifnot(is.numeric(lower))
  stopifnot(length(lower) == 1)
  stopifnot(is.numeric(upper))
  stopifnot(length(upper) == 1)
  stopifnot(lower <= upper)
  # Verify start
  if (is.null(start)) {
    start <- (lower+upper) / 2
  } else {
    stopifnot(is.numeric(start))
    stopifnot(length(start) == 1)
    stopifnot(lower <= start)
    stopifnot(start <= upper)
  }

  out <- list(lower=lower,
              upper=upper,
              start=start)
  out$sample <- function(n) {
    runif(n, lower, upper)
  }
  class(out) <- c("mixopt_par", "mixopt_par_cts", class(out))
  out
}

#' Ordered variable parameter
#'
#' @param values Values the parameter can take, in order
#' @param start Start parameter for optimization
#'
#' @return mixopt_par list
#' @export
#'
#' @examples
#' mopar_ordered(c(1,3,5))
#' #mopar_ordered(c(1,1))
#' mopar_ordered(c('a','c'))
#' mopar_ordered(1:4)
#' mopar_ordered(4:1)
#' mopar_ordered(list('a', 2, 'c', sin))
mopar_ordered <- function(values, start=NULL) {
  stopifnot(length(values) >= 1)
  # stopifnot(anyDuplicated(values))
  if(anyDuplicated(values) > .5) {
    # browser()
    print(values)
    print("mopar_unordered has duplicated values")
  }
  if (is.null(start)) {
    start <- values[[ceiling(length(values) / 2)]]
  }
  stopifnot(start %in% values)
  out <- list(values=values, start=start)
  out$sample <- function(n) {
    if (length(values) == 1) {
      rep(values, n)
    } else {
      sample(values, n)
    }
  }
  class(out) <- c("mixopt_par", "mixopt_par_ordered", class(out))
  out
}

#' Unordered factor parameter
#'
#' @param values Values the variable can take
#' @param start Start value. Chosen randomly if not given.
#'
#' @return mixopt_par list
#' @export
#'
#' @examples
#' mopar_unordered(c(1,3,9))
#' mopar_unordered(letters)
mopar_unordered <- function(values, start=NULL) {
  stopifnot(length(values) >= 1)
  if(anyDuplicated(values) > .5) {
    # browser()
    print(values)
    print("mopar_unordered has duplicated values")
  }
  if (is.null(start)) {
    start <- values[[sample(1:length(values), 1)]]
  }
  stopifnot(start %in% values)
  out <- list(values=values, start=start)
  out$sample <- function(n) {
    if (length(values) == 1) {
      rep(values, n)
    } else {
      sample(values, n)
    }
  }
  class(out) <- c("mixopt_par", "mixopt_par_unordered", class(out))
  out
}

#' @export
c.mixopt_par <- function(..., recursive=FALSE) {
  out <- list()
  dots <- list(...)
  for (i in 1:length(dots)) {
    out[[i]] <- dots[[i]]
  }
  out
}
