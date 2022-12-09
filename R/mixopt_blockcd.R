#' @export
#'
#' @rdname mixopt
#' @param maxtime Maximum time to run in seconds. Not an exact limit, only
#' checks occasionally.
#' @param control Parameters for optimizing.
#' @param maxblocksize The maximum number of continuous dimensions that should
#' be placed into a single block.
#'
#' @references https://en.wikipedia.org/wiki/Coordinate_descent
#'
#' @examples
#' # Simple 1D example
#' mixopt_blockcd(par=list(mopar_cts(2,8)), fn=function(x) {(4.5-x[1])^2})
#' # With gradient (isn't faster)
#' mixopt_blockcd(par=list(mopar_cts(2,8)), fn=function(x) {(4.5-x[1])^2},
#'                gr=function(x) {-2*(4.5-x[1])})
#'
#' # 1D discrete ordered
#' mixopt_blockcd(par=list(mopar_ordered(100:10000)),
#'                  fn=function(x) {(x[1] - 500.3)^2})
#'
#' # 2D: one continuous, one factor
#' mixopt_blockcd(par=list(mopar_cts(2,8), mopar_unordered(letters[1:6])),
#'                  fn=function(x) {ifelse(x[2] == 'b', -1, 0) +
#'                                  (4.5-x[1])^2})
mixopt_blockcd <- function(par, fn, gr=NULL, ...,
                           control=list(),
                           maxblocksize=NULL,
                           method,
                           maxiter=100, maxeval=NULL,
                           maxtime=NULL,
                           verbose=0,
                           track=FALSE) {
  # print(par)

  # If 1-D, par can just be the par instead of a list of par
  if ("mixopt_par" %in% class(par)) {
    par <- list(par)
  }
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
  if (is.null(maxtime)) {
    maxtime <- Inf
  }
  stopifnot(!is.null(maxtime), is.numeric(maxtime),
            length(maxtime) == 1, maxtime >= 0)
  stopifnot(is.null(maxblocksize) ||
              (is.numeric(maxblocksize) && length(maxblocksize) == 1))
  # Check control
  stopifnot(is.list(control))
  if (is.null(control$reltol)) {
    reltol <- sqrt(.Machine$double.eps)
  } else {
    stopifnot(is.numeric(reltol), length(reltol)==1)
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

  # Convert to mixopt_list
  # class(par_par) <- c("mixopt_list", class(par_par))
  # but only if it can't be kept as numeric/char
  par_par <- as.mixopt_list(par_par, TRUE)

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

  # Set up blocks ----
  partypes <- sapply(par, function(m) {class(m)[2]})
  stopifnot(length(partypes)==length(par), is.character(partypes),
            partypes %in% c('mixopt_par_cts', 'mixopt_par_ordered',
                            'mixopt_par_unordered'))
  nblock <- 0
  blockinds <- list()
  blockclass <- character()
  inds_cts <- which(partypes == 'mixopt_par_cts')
  inds_ord <- which(partypes == 'mixopt_par_ordered')
  inds_unord <- which(partypes == 'mixopt_par_unordered')
  stopifnot(length(inds_cts) + length(inds_ord) +
              length(inds_unord) == length(par))
  # All cts go into first block unless maxblocksize
  if (length(inds_cts) > .5) {
    if (is.null(maxblocksize)) {
      nblock <- nblock + 1
      blockinds[[nblock]] <- inds_cts
      blockclass[[nblock]] <- "mixopt_par_cts"
    } else {
      # browser()
      numctsblocks <- ceiling(length(inds_cts) / floor(maxblocksize))
      stopifnot(numctsblocks >= 1,
                all.equal(numctsblocks, round(numctsblocks)))
      ctsassignments <- sample(rep(1:numctsblocks,
                                   maxblocksize)[1:length(inds_cts)],
                               length(inds_cts),
                               replace=FALSE)
      for (i in 1:numctsblocks) {
        nblock <- nblock + 1
        blockinds[[nblock]] <- inds_cts[ctsassignments == i]
        blockclass[[nblock]] <- "mixopt_par_cts"
      }
      if (verbose >= 6) {
        cat("cts block assignments are\n")
        print(blockinds)
      }
    }
  }
  # Ordered and unordered go into own blocks
  for (i in seq_along(inds_ord)) {
    nblock <- nblock + 1
    blockinds[[nblock]] <- inds_ord[i]
    blockclass[[nblock]] <- "mixopt_par_ordered"
  }
  for (i in seq_along(inds_unord)) {
    nblock <- nblock + 1
    blockinds[[nblock]] <- inds_unord[i]
    blockclass[[nblock]] <- "mixopt_par_unordered"
  }

  stopifnot(is.character(blockclass),
            blockclass %in% c('mixopt_par_cts', 'mixopt_par_ordered',
                              'mixopt_par_unordered'),
            sort(unlist(blockinds)) == 1:length(par))

  iter <- 0
  counts_function <- 1 # Evaluated once above
  counts_gradient <- 0
  starttime <- Sys.time()
  # Iterate with while loop ----
  # An iteration goes over each variable separately
  while(iter <= maxiter &&
        counts_function < maxeval &&
        as.numeric(Sys.time() - starttime, units='secs') < maxtime) {
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
    for (iblock in 1:nblock) {
      inds_iblock <- blockinds[[iblock]]
      best_val_sofar <- min(par_val, best_val_sofar_input)
      fnipar <- function(pari) {
        x <- par_par
        x[inds_iblock] <- pari
        fnx <- fn(x)

        if (verbose >= 10) {
          if (is.na(pari)) {
            stop("pari is NA in coorddesc")
          }
          cat("  iblock=", iblock, " set at ", pari, " evaluates to ",
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
      if (blockclass[iblock] == "mixopt_par_cts") {
        ## cts ----
        # Use gradient
        if (is.null(gr)) {
          gripar=NULL
        } else {
          gripar <- function(pari) {
            x <- par_par
            x[inds_iblock] <- pari
            grxall <- gr(x)
            grx <- grxall[inds_iblock]

            if (verbose >= 10) {
              if (is.na(pari)) {
                stop("pari is NA in coorddesc gr")
              }
              cat("  iblock=", iblock, " set at ", pari, " grad evaluates to ",
                  signif(grx, 8), "\n")
            }

            counts_gradient <<- counts_gradient + 1
            # if (track) {
            #   tracked_pars[[length(tracked_pars) + 1]] <<- x
            #   tracked_vals[[length(tracked_vals) + 1]] <<- fnx
            #   tracked_newbest[[length(tracked_newbest) + 1]] <<-
            #     (fnx < best_val_sofar)
            # }
            grx
          }
        }
        # Optimize over all cts dims
        control_list <- list()
        if (nblock > 1.5) {control_list$maxit=30}
        optout <- optim(par=par_par[blockinds[[iblock]]],
                        fn=fnipar,
                        gr=gripar,
                        method="L-BFGS-B",
                        # lower=par[[ipar]]$lower,
                        # upper=par[[ipar]]$upper,
                        lower=sapply(blockinds[[iblock]],
                                     function(j) {par[[j]]$lower}),
                        upper=sapply(blockinds[[iblock]],
                                     function(j) {par[[j]]$upper}),
                        control=control_list)

        par_par[blockinds[[iblock]]] <- optout$par
        par_val <- optout$val
      } else if (blockclass[iblock] == "mixopt_par_ordered") {
        ipar <- blockinds[[iblock]]
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
      } else if (blockclass[iblock] == "mixopt_par_unordered") {
        ipar <- blockinds[[iblock]]
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
      } # End all par options

      # Break if exceeded max function evals
      if (counts_function >= maxeval) {
        break
      }
      # Break if exceeded maxtime
      if (as.numeric(Sys.time() - starttime, units='secs') > maxtime) {
        if (verbose >= 4) {
          cat("Breaking coorddesc b/c exceeded maxtime\n")
        }
        break
      }
    } # end for (iblock in 1:nblocks)

    if (par_val_before < par_val) {
      warning(paste0("par_val_before < par_val", par_val_before, par_val))
    }
    # Break if no improvement
    if (par_val >= par_val_before ||
        (((par_val_before - par_val) / par_val_before) <
         reltol * (abs(par_val) + reltol))) {
      if (verbose >= 3) {
        cat("No improvement, breaking while loop\n")
      }
      break
    }
    # Break if only one block and it is cts
    if (nblock == 1 && blockclass[1] == "mixopt_par_cts") {
      if (verbose >= 6) {
        cat("Only 1 block and cts, breaking\n")
      }
      break
    }
  }
  # Completed optim ----
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
  outlist$counts <- c("function"=counts_function, "gradient"=counts_gradient)
  outlist$runtime <- endtime - starttime
  # Add class
  class(outlist) <- c("mixopt_output_list", class(outlist))
  # Return list
  outlist
}
