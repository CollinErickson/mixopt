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
                   verbose=0,
                   track) {
  return(mixopt_multistart(par=par, gr=gr, method=method, ...))
}

#' @export
print.mixopt_output_list <- function(x, ...) {
  x2 <- x[setdiff(names(x), c("track"))]
  class(x2) <- setdiff(class(x2), "mixopt_output_list")
  print(x2)
  invisible(x2)
}
