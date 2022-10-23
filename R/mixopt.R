#' Mixed variable optimization using coordinate descent
#'
#' @param par List of parameters
#' @param fn Function to evaluate
#' @param gr Gradient of fn
#' @param ... Additional args
#' @param method Optimization method
#' @param maxiter Maximum number of outer iterations.
#' For coordinate descent, one iteration is a loop over each parameter.
#' @param maxeval Maximum number of function evaluations.
#' It may go over this number while in an inner optimization loop,
#' but will exit after that.
#' @param verbose How much to print. 0 is none, 1 is standard,
#' 2 is some, 3 is a lot, etc.
#' @param track Should it track the parameters evaluated and value?
#' @param global Global optimization algorithm to use.
#' `FALSE` if you only want local optimization.
#' @param local Local optimization algorithm to use.
#' @param track Should it track the parameters evaluated and value?
#' @importFrom stats optim
#'
#' @return List
mixopt <- function(par, fn, gr=NULL,
                   global="multistart",
                   local="coorddesc",
                   ...,
                   method,
                   verbose=10,
                   track) {
  return(mixopt_coorddesc(par=par, gr=gr, method=method, ...))
}

#' @rdname mixopt
#' @param n0 For multistart, number of random initial points to evaluate.
#' @param n1 For multistart, number of best starts to optimize with.
#' You should have `n0` less than `n1`, potentially by a large factor.
#' gradient descent.
#' @export
#' @examples
#' # 2D
#' library(ggplot2)
#' library(dplyr)
#' f6 <- function(x) {-(-x[[1]]*.5*sin(.5*x[[1]])*1 - 1e-2*x[[2]]^2 + .2*x[[1]] - .3*x[[2]])}
#' ContourFunctions::cf_func(f6, xlim=c(0,100), ylim=c(-100,100))
#' m6 <- mixopt_coorddesc(par=list(mopar_cts(0,100), mopar_cts(-100,100)),
#'                        fn=f6, track = TRUE)
#' plot_track(m6)
#' ms6 <- mixopt_multistart(par=list(mopar_cts(0,100), mopar_cts(-100,100)),
#'                          fn=f6, track = TRUE)
#' plot_track(ms6)
#' ContourFunctions::cf_func(f6, xlim=c(0,100), ylim=c(-100,100),
#'                           gg = TRUE) +
#'   geom_point(data=as.data.frame(matrix(unlist(ms6$track$par), ncol=2, byrow=TRUE)) %>%
#'                bind_cols(newbest=ms6$track$newbest),
#'              aes(V1, V2, color=newbest), alpha=.5)
mixopt_multistart <- function(par, fn, gr=NULL,
                              ..., method,
                              n0=20, n1=2,
                              maxiter=100, verbose=10,
                              track=FALSE) {
  # Start by evaluating n0 points, pick them randomly
  stopifnot(is.numeric(n0), length(n0) == 1, n0 >= 1,
            abs(n0 - as.integer(n0)) < 1e-8)
  stopifnot(is.numeric(n1), length(n1) == 1, n1 >= 1,
            abs(n1 - as.integer(n1)) < 1e-8)
  stopifnot(n0 >= n1)
  # browser()
  starttime <- Sys.time()
  counts_function <- 0

  if (track) {
    tracked_pars <- list()
    tracked_vals <- numeric(0)
    tracked_newbest <- logical(0)
  }

  startpoints <- list()
  for (ivar in 1:length(par)) {
    startpoints[[ivar]] <- par[[ivar]]$sample(n0)
  }
  startpoints2 <- list()
  startpointsval <- rep(NaN, n0)
  # Global points loop ----
  for (i in 1:n0) {
    # Generate start points
    startpoints2[[i]] <- lapply(startpoints, function(x) {x[[i]]})
    startpointsval[[i]] <- fn(startpoints2[[i]])
    counts_function <- counts_function + 1
    if (track) {
      # browser()
      tracked_pars[[length(tracked_pars) + 1]] <- startpoints2[[i]]
      tracked_vals[[length(tracked_vals) + 1]] <- startpointsval[[i]]
      tracked_newbest[[length(tracked_newbest) + 1]] <- (if (i==1) {T} else {startpointsval[[i]] < min(startpointsval[1:(i-1)])})
    }
  }

  # browser()
  # Find best
  # ranks <- order(order(startpointsval))
  n0_inds_sorted <- order(startpointsval)
  n1_inds <- n0_inds_sorted[1:n1]

  # Local search ----
  # Run local optimizer over the n1 best
  locoptouts <- list()
  # browser()
  for (i in 1:n1) {
    # pars_i <-
    # Set start points
    for (ivar in 1:length(par)) {
      par[[ivar]]$start <- startpoints2[[n1_inds[[i]]]][[ivar]]
    }

    # Run local optimizer
    locoptouts[[i]] <- mixopt_coorddesc(par=par, fn=fn, gr=gr, track=track,
                                        best_val_sofar=if (track) {min(tracked_vals)} else {Inf})
    counts_function <- counts_function + locoptouts[[i]]$counts[['function']]
    if (track) {
      # browser()
      tracked_pars <- c(tracked_pars, locoptouts[[i]]$track$par)
      tracked_vals <- c(tracked_vals, locoptouts[[i]]$track$val)
      tracked_newbest <- c(tracked_newbest, locoptouts[[i]]$track$newbest)
    }
  }

  n1_vals <- sapply(locoptouts, function(x) {x$val})
  best_n1_ind <- which.min(n1_vals)[1]

  # browser()
  # Pick best
  outlist <- locoptouts[[best_n1_ind]][c("par", "val")]
  endtime <- Sys.time()

  if (track) {
    if (any(diff(c(length(tracked_pars), length(tracked_vals),
                   length(tracked_newbest))) != 0)) {
      warning("Tracking has bad length #982367")
    }
    outlist$track <- list(
      par=tracked_pars,
      val=tracked_vals,
      newbest=tracked_newbest
    )
  }

  outlist$counts <- c("function"=counts_function, "gradient"=NA)
  outlist$runtime <- endtime - starttime

  # Add class
  class(outlist) <- c("mixopt_output_list", class(outlist))

  # Return list
  outlist
}

#' @export
#'
#' @rdname mixopt
#'
#' @examples
#' # Simple 1D example
#' mixopt_coorddesc(par=list(mopar_cts(2,8)), fn=function(x) {(4.5-x[[1]])^2})
#'
#' # 1D discrete ordered
#' mixopt_coorddesc(par=list(mopar_ordered(100:10000)),
#'                  fn=function(x) {(x[[1]] - 500.3)^2})
#'
#' # 2D: one continuous, one factor
#' mixopt_coorddesc(par=list(mopar_cts(2,8), mopar_unordered(letters[1:6])),
#'                  fn=function(x) {ifelse(x[[2]] == 'b', -1, 0) +(4.5-x[[1]])^2})
mixopt_coorddesc <- function(par, fn, gr=NULL, ..., method,
                             maxiter=100, maxeval=NULL,
                             verbose=10,
                             track=FALSE) {
  # print(par)

  # Verify that par are valid
  verify_par(par)
  if (verbose>=2) {
    cat("par are verified\n")
  }
  npar <- length(par)

  # Check other inputs
  stopifnot(is.numeric(maxiter), length(maxiter) == 1, maxiter >= 1,
            abs(maxiter - as.integer(maxiter)) < 1e-8)
  if (missing(maxeval) || is.null(maxeval)) {
    maxeval <- Inf
  } else {
    stopifnot(is.numeric(maxeval), length(maxeval) == 1, maxeval >= 1,
              abs(maxeval - as.integer(maxeval)) < 1e-8)
  }

  # Set up tracking
  if (track) {
    tracked_pars <- list()
    tracked_vals <- numeric(0)
    tracked_newbest <- logical(0)
  }

  # Evaluate initial points
  par_par <- lapply(par, function(p) {p$start})
  stopifnot(length(par_par) == npar)
  # browser()
  # par_val <- Inf

  # Evaluate initial parameter if first to evaluated
  # Only problem is that optim will duplicate it
  # browser()
  # if (iter == 1 && is.infinite(par_val)) {
  # print("INITIALIZING away from Inf")
  # par_val <- fnipar(par_par[[ipar]])
  par_val <- fn(par_par)
  # }

  if (track) {
    # tracked_pars <- list()
    # tracked_vals <- numeric(0)
    # tracked_newbest <- logical(0)
    tracked_pars <- list(par_par)
    tracked_vals <- par_val
    # browser()
    dots <- list(...)
    if ("best_val_sofar" %in% names(dots)) {
      # browser()
      best_val_sofar_input <- dots$best_val_sofar
      tracked_newbest <- (par_val < best_val_sofar_input)
    } else {
      best_val_sofar_input <- Inf
      tracked_newbest <- TRUE
    }
  } else {
    best_val_sofar_input <- Inf
  }

  iter <- 0
  counts_function <- 1 # Evaluated once above
  starttime <- Sys.time()
  # Iterate with while loop ----
  # An iteration goes over each variable separately
  while(iter <= maxiter && counts_function < maxeval) {
    iter <- iter + 1
    par_val_before <- par_val
    if (verbose >= 2) {
      cat("Starting coordinate descent iteration", iter, ", val is",
          par_val, "", "\n")
    }
    if (verbose >= 5) {
      print(par_par)
    }
    # browser()
    # Loop over pars ----
    for (ipar in 1:npar) {
      best_val_sofar <- min(par_val, best_val_sofar_input)
      fnipar <- function(pari) {
        # browser()
        x <- par_par
        x[[ipar]] <- pari
        fnx <- fn(x)

        if (verbose >= 10) {
          if (is.na(pari)) {
            # browser()
            stop("pari is NA in coorddesc")
          }
          cat("  ipar=", ipar, " set at ", pari, " evaluates to ",
              signif(fnx, 8), "\n")
        }

        counts_function <<- counts_function + 1
        if (track) {
          tracked_pars[[length(tracked_pars) + 1]] <<- x
          tracked_vals[[length(tracked_vals) + 1]] <<- fnx
          tracked_newbest[[length(tracked_newbest) + 1]] <<- (fnx < best_val_sofar)
        }
        fnx
      }
      if ("mixopt_par_cts" %in% class(par[[ipar]])) {
        ## cts ----
        # Optimize over 1-D
        optout <- optim(par=par_par[[ipar]], fn=fnipar,
                        method="L-BFGS-B",
                        lower=par[[ipar]]$lower,
                        upper=par[[ipar]]$upper,
                        control=list(maxit=30))

        par_par[[ipar]] <- optout$par
        par_val <- optout$val
      } else if ("mixopt_par_ordered" %in% class(par[[ipar]])) {
        ## ordered ----
        # Optimize over param
        # browser()
        if (length(par[[ipar]]$values) > 1.5) {
          # Get index of current value
          startind <- which(par_par[[ipar]] == par[[ipar]]$values)
          stopifnot(length(startind) == 1)

          if (F) {
            stop("Not using this way anymore")
            # Old way would go 1 index at a time
            # Try moving in one direction
            # dir <- sample(c(-1, 1), 1)
            curind <- startind #+ 1
            # Check left and right
            if (curind > 1.5) {
              fnleft <- fnipar(par[[ipar]]$values[curind - 1])
            } else {
              fnleft <- Inf
            }
            if (curind < length(par[[ipar]]$values) - .5) {
              fnright <- fnipar(par[[ipar]]$values[curind + 1])
            } else {
              fnright <- Inf
            }
            # Go the better of left/right
            if (fnleft <= fnright && fnleft < par_val) {
              par_par[[ipar]] <- par[[ipar]]$values[curind - 1]
              par_val <- fnleft
              # Keep going left
              curind <- curind - 1
              while (curind > 1.5) {
                keepleftval <- fnipar(par[[ipar]]$values[curind - 1])
                if (keepleftval < par_val) {
                  par_par[[ipar]] <- par[[ipar]]$values[curind - 1]
                  par_val <- keepleftval
                  curind <- curind - 1
                } else {
                  break
                }
              }
            } else if (fnright <= fnleft && fnright < par_val) {
              # Keep going right
              curind <- curind + 1
              while (curind < length(par[[ipar]]$values) - .5) {
                keeprightval <- fnipar(par[[ipar]]$values[curind + 1])
                if (keeprightval < par_val) {
                  par_par[[ipar]] <- par[[ipar]]$values[curind + 1]
                  par_val <- keeprightval
                  curind <- curind + 1
                } else {
                  break
                }
              }
            }
            # End old way
          } else {
            # New way calls function
            # browser()
            fils_out <- full_index_line_search(f=fnipar,
                                   xarray=par[[ipar]]$values,
                                   startind=startind,
                                   verbose=verbose,
                                   ystart=par_val)

            par_par[[ipar]] <- fils_out$x
            par_val <- fils_out$val

          }
        } # End at least 2 values.
        # Else only single value, don't do anything.
      } else if ("mixopt_par_unordered" %in% class(par[[ipar]])) {
        ## unordered ----
        # browser()
        # Randomly try other param values
        param_values <- setdiff(par[[ipar]]$values, par_par[[ipar]])
        # Limit it to a small number
        if (length(param_values) >= 10) {
          param_values <- sample(param_values, 10)
        }
        if (length(param_values) > 1.5) {
          for (iii in 1:length(param_values)) {
            iii_val <- fnipar(param_values[[iii]])
            if (iii_val < par_val) {
              # browser()
              if (verbose>=4) {
                cat("New min from unordered param", param_values[[iii]], "\n")
              }
              par_par[[ipar]] <- param_values[[iii]]
              par_val <- iii_val
            }
          }
        }
      } else {
        stop("bad par")
      }

      # Break if exceeded max function evals
      if (counts_function >= maxeval) {
        break
      }
    } # end for (ipar in 1:npar)

    # Break if no improvement
    if (par_val >= par_val_before) {
      if (verbose >= 3) {
        cat("No improvement, breaking while loop\n")
      }
      break
    }
  }
  endtime <- Sys.time()

  outlist <- list(par=par_par,
                  val=par_val)
  if (track) {
    # outlist$track_par <- tracked_pars
    # outlist$track_val <- tracked_vals
    # outlist$track_newbest <- tracked_newbest
    if (any(diff(c(length(tracked_pars), length(tracked_vals),
                   length(tracked_newbest))) != 0)) {
      warning("Tracking has bad length #19351378")
    }
    outlist$track <- list(
      par=tracked_pars,
      val=tracked_vals,
      newbest=tracked_newbest
    )
  }
  outlist$counts <- c("function"=counts_function, "gradient"=NA)
  outlist$runtime <- endtime - starttime
  # Add class
  class(outlist) <- c("mixopt_output_list", class(outlist))
  # Return list
  outlist
}

#' @export
print.mixopt_output_list <- function(x, ...) {
  # browser()
  # print('printing new thing')
  # x2 <- x[setdiff(names(x), c("track_par", "track_val"))]
  x2 <- x[setdiff(names(x), c("track"))]
  class(x2) <- setdiff(class(x2), "mixopt_output_list")
  print(x2)
}
