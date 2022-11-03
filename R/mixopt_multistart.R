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
#' f6 <- function(x) {-(-x[[1]]*.5*sin(.5*x[[1]])*1 - 1e-2*x[[2]]^2 +
#'                        .2*x[[1]] - .3*x[[2]])}
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
  starttime <- Sys.time()
  counts_function <- 0

  if (track) {
    tracked_pars <- list()
    tracked_vals <- numeric(0)
    tracked_newbest <- logical(0)
  }

  startpoints <- list()
  startpointslhs <- list()
  lhsq <- lhs::maximinLHS(n=n0, k=length(par))
  for (ivar in 1:length(par)) {
    startpoints[[ivar]] <- par[[ivar]]$sample(n0)
    startpointslhs[[ivar]] <- par[[ivar]]$q(lhsq[, ivar])
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
      tracked_pars[[length(tracked_pars) + 1]] <- startpoints2[[i]]
      tracked_vals[[length(tracked_vals) + 1]] <- startpointsval[[i]]
      tracked_newbest[[length(tracked_newbest) + 1]] <- (if (i==1) {T} else {startpointsval[[i]] < min(startpointsval[1:(i-1)])})
    }
  }

  # Find best
  # ranks <- order(order(startpointsval))
  n0_inds_sorted <- order(startpointsval)
  n1_inds <- n0_inds_sorted[1:n1]

  # Local search ----
  # Run local optimizer over the n1 best
  locoptouts <- list()
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
      tracked_pars <- c(tracked_pars, locoptouts[[i]]$track$par)
      tracked_vals <- c(tracked_vals, locoptouts[[i]]$track$val)
      tracked_newbest <- c(tracked_newbest, locoptouts[[i]]$track$newbest)
    }
  }

  n1_vals <- sapply(locoptouts, function(x) {x$val})
  best_n1_ind <- which.min(n1_vals)[1]

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
