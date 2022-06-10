mixopt <- function(par, fn, gr=NULL, ...,
                   method,
                   track_par) {
  return(mixopt_coorddesc(par=par, gr=gr, method=method, ...))
}

#' Mixed variable optimization using coordinate descent
#'
#' @param par List of parameters
#' @param fn Function to evaluate
#' @param gr Gradient of fn
#' @param ... Additional args
#' @param method Optimization method
#' @param maxiter Maximum number of outer iterations
#' @param verbose How much to print. 0 is none, 1 is standard,
#' 2 is some, 3 is a lot, etc.
#' @param track_par Should it track the parameters evaluated?
#' @importFrom stats optim
#'
#' @return List
#' @export
#'
#' @examples
#' mixopt_coorddesc(par=list(par_cts(2,8)), fn=function(x) {(4.5-x[[1]])^2})
#' mixopt_coorddesc(par=list(par_cts(2,8), par_unordered(letters[1:6])),
#'                  fn=function(x) {ifelse(x[[2]] == 'b', -1, 0) +(4.5-x[[1]])^2})
mixopt_coorddesc <- function(par, fn, gr=NULL, ..., method,
                             maxiter=100, verbose=10,
                             track_par=FALSE) {
  # print(par)
  verify_par(par)
  if (verbose>=2) {
    cat("par are verified\n")
  }
  npar <- length(par)

  par_par <- lapply(par, function(p) {p$start})
  par_val <- Inf
  stopifnot(length(par_par) == npar)
  iter <- 0
  # An iteration goes over each variable separately
  while(iter <= maxiter) {
    iter <- iter + 1
    par_val_before <- par_val
    cat("Starting iter", iter, ", val is", par_val, "", "\n")
    print(par_par)
    # browser()
    # Loop over parameters/variables
    for (ipar in 1:npar) {
      fnipar <- function(pari) {
        # browser()
        x <- par_par
        x[[ipar]] <- pari
        fn(x)
      }
      if ("mixopt_par_cts" %in% class(par[[ipar]])) {
        # Optimize over 1-D
        optout <- optim(par=par_par[[ipar]], fn=fnipar,
                        method="Brent",
                        lower=par[[ipar]]$lower,
                        upper=par[[ipar]]$upper)

        par_par[[ipar]] <- optout$par
        par_val <- optout$val
      } else if ("mixopt_par_ordered" %in% class(par[[ipar]])) {
        browser()
        # Get index of current value
        curind <- which()


        par_par[[ipar]] <- optout$par
        par_val <- optout$val
      } else if ("mixopt_par_unordered" %in% class(par[[ipar]])) {
        # browser()
        # Randomly try other param values
        param_values <- setdiff(par[[ipar]]$values, par_par[[ipar]])
        # Limit it to a small number
        if (length(param_values) >= 10) {
          param_values <- sample(param_values, 10)
        }
        if (length(param_values) > .5) {
          for (iii in 1:length(param_values)) {
            iii_val <- fnipar(param_values[[iii]])
            if (iii_val < par_val) {
              # browser()
              cat("New min from unordered param\n")
              par_par[[ipar]] <- param_values[[iii]]
              par_val <- iii_val
            }
          }
        }
      } else {
        stop("bad par")
      }
    }
    if (par_val >= par_val_before) {
      cat("No improvement, breaking while loop\n")
      break
    }
  }

  outlist <- list(par=par_par,
                  val=par_val)
  outlist
}
