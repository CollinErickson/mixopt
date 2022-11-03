
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
    if (!("mixopt_par" %in% class(par[[i]]))) {
      stop("Error: mixopt_par not in class for par[[i]]")
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
  out$q <- function(p) {
    stopifnot(p>=0, p<=1)
    lower + p * (upper - lower)
  }
  class(out) <- c("mixopt_par", "mixopt_par_cts", class(out))
  out
}

#' @export
print.mixopt_par_cts <- function(x, ...) {
  s <- paste0("mopar_cts(lower = ", x$lower, ", ",
              "upper = ", x$upper, ", ",
              "start = ", x$start, ")\n")
  cat(s)
  invisible(x)
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
    print(values)
    stop("mopar_unordered has duplicated values")
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
      sample(values, n, replace = T)
    }
  }
  out$q <- function(p) {
    stopifnot(p>=0, p<=1)
    values[1 + floor(length(values) * p*.999999999)]
  }
  class(out) <- c("mixopt_par", "mixopt_par_ordered", class(out))
  out
}

#' @export
print.mixopt_par_ordered <- function(x, ...) {
  print_mixopt_par_ordered_or_unordered(x, ...)
}

#' @export
print.mixopt_par_unordered <- function(x, ...) {
  print_mixopt_par_ordered_or_unordered(x, ...)
}

print_mixopt_par_ordered_or_unordered <- function(x, ...) {
  if ("mixopt_par_ordered" %in% class(x)) {
    s <- paste0(
      "mopar_ordered with ", length(x$values)," values\n"
    )
  } else if ("mixopt_par_unordered" %in% class(x)) {
    s <- paste0(
      "mopar_unordered with ", length(x$values)," values\n"
    )
  } else {
    stop("Bad class passed to print_mixopt_par_ordered_or_unordered")
  }
  # co <- capture.output(print(x[c("values", "start")]))
  # # Indent each
  # tcon <- paste0("\t", co) #, "\n")
  # tcon1 <- paste0(tcon, collapse = "\n")
  # s <- paste(s, tcon1)
  s <- paste0(s, "\t values = ")
  if (length(x$values) <= 50) {
    # s <- paste0(s,
    #             # "\t",
    #             capture.output(print(x$values,
    #                                  width=getOption("width")-15)),
    #             "\n")
    for (i in 1:length(x$values)) {
      if (i > 1.5) {
        s <- paste0(s, " ")
      }
      if (is.numeric(x$values[i])) {
        s <- paste0(s, signif(x$values[i], 6))
      } else {
        s <- paste0(s, x$values[i])
      }
    }
  } else { # Large size, only do some at beginning and end
    # s <- paste0(s, "\t")
    for (i in 1:10) {
      if (i > 1.5) {
        s <- paste0(s, " ")
      }
      if (is.numeric(x$values[i])) {
        s <- paste0(s, signif(x$values[i], 6))
      } else {
        s <- paste0(s, x$values[i])
      }
    }
    s <- paste0(s, " ... (", length(x$values) -20,
                " more values) ...")
    for (i in (length(x$values)-9):length(x$values)) {
      # if (i > length(x$values)-9+.5) {
      s <- paste0(s, " ")
      # }
      if (is.numeric(x$values[i])) {
        s <- paste0(s, signif(x$values[i], 6))
      } else {
        s <- paste0(s, x$values[i])
      }
      # if (i < length(x$values)-.5) {
      #   s <- paste0(s, "")
      # }
    }
  }
  s <- paste0(s, "\n\tstart = ", x$start, "\n")
  # s <- paste0(s, )
  cat(s)
  invisible(x)
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
    print(values)
    stop("mopar_unordered has duplicated values")
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
      sample(values, n, replace = TRUE)
    }
  }
  out$q <- function(p) {
    stopifnot(p>=0, p<=1)
    values[1 + floor(length(values) * p*.999999999)]
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
  class(out) <- c("mixopt_par_list", class(out))
  out
}

if (F) {
  # Check that combine and print work
  p1 <- mopar_cts(-1,1)
  p2 <- mopar_unordered(letters)
  p3 <- mopar_ordered(runif(100))
  c(p1, p2, p3)
}

#' @export
#' @importFrom utils capture.output
print.mixopt_par_list <- function(x, ...) {
  s <- paste0("List of ", length(x), " mixopt pars\n")
  for (i in 1:length(x)) {
    s <- paste0(s, "\t[[", i, "]]")
    xico <- capture.output(print(x[[i]]))
    xico2 <- paste0("\t", xico, collapse = "\n")
    s <- paste0(s, xico2, "\n") #"\t[[", i, "]]")
  }
  cat(s)
  invisible(x)
}
