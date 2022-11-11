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
#'                  fn=function(x) {ifelse(x[[2]] == 'b', -1, 0) +
#'                                  (4.5-x[[1]])^2})
mixopt_coorddesc <- function(par, fn, gr=NULL, ..., method,
                             maxiter=100, maxeval=NULL,
                             verbose=0,
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
  # par_val <- Inf

  # Evaluate initial parameter if first to evaluated
  # Only problem is that optim will duplicate it
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
    dots <- list(...)
    if ("best_val_sofar" %in% names(dots)) {
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
    # Loop over pars ----
    for (ipar in 1:npar) {
      best_val_sofar <- min(par_val, best_val_sofar_input)
      fnipar <- function(pari) {
        x <- par_par
        x[[ipar]] <- pari
        fnx <- fn(x)

        if (verbose >= 10) {
          if (is.na(pari)) {
            stop("pari is NA in coorddesc")
          }
          cat("  ipar=", ipar, " set at ", pari, " evaluates to ",
              signif(fnx, 8), "\n")
        }

        counts_function <<- counts_function + 1
        if (track) {
          tracked_pars[[length(tracked_pars) + 1]] <<- x
          tracked_vals[[length(tracked_vals) + 1]] <<- fnx
          tracked_newbest[[length(tracked_newbest) + 1]] <<-
            (fnx < best_val_sofar)
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
        if (length(par[[ipar]]$values) > 1.5) {
          # Get index of current value
          startind <- which(par_par[[ipar]] == par[[ipar]]$values)
          stopifnot(length(startind) == 1)

          fils_out <- full_index_line_search(f=fnipar,
                                             xarray=par[[ipar]]$values,
                                             startind=startind,
                                             verbose=verbose,
                                             ystart=par_val)

          par_par[[ipar]] <- fils_out$x
          par_val <- fils_out$val

        } # End at least 2 values.
        # Else only single value, don't do anything.
      } else if ("mixopt_par_unordered" %in% class(par[[ipar]])) {
        ## unordered ----
        # Randomly try other param values b/c no info from order
        param_values <- setdiff(par[[ipar]]$values, par_par[[ipar]])
        # Limit it to a small number
        if (length(param_values) >= 10) {
          param_values <- sample(param_values, 10)
        }
        if (length(param_values) > 1.5) {
          for (iii in 1:length(param_values)) {
            iii_val <- fnipar(param_values[[iii]])
            if (iii_val < par_val) {
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
