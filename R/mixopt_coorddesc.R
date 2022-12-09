#' @export
#'
#' @rdname mixopt
#' @param maxtime Maximum time to run in seconds. Not an exact limit, only
#' checks occasionally.
#'
#' @references https://en.wikipedia.org/wiki/Coordinate_descent
#'
#' @examples
#' # Simple 1D example
#' mixopt_coorddesc(par=list(mopar_cts(2,8)), fn=function(x) {(4.5-x[1])^2})
#'
#' # 1D discrete ordered
#' mixopt_coorddesc(par=list(mopar_ordered(100:10000)),
#'                  fn=function(x) {(x[1] - 500.3)^2})
#'
#' # 2D: one continuous, one factor
#' mixopt_coorddesc(par=list(mopar_cts(2,8), mopar_unordered(letters[1:6])),
#'                  fn=function(x) {ifelse(x[2] == 'b', -1, 0) +
#'                                  (4.5-x[1])^2})
mixopt_coorddesc <- function(par, fn, gr=NULL, ..., method,
                             maxiter=100, maxeval=NULL,
                             maxtime=NULL,
                             verbose=0,
                             track=FALSE) {
  # Now this function just calls blockcd with maxblocksize=1 to avoid
  # maintaining essentially the same code in two places.
  return(
    mixopt_blockcd(
      maxblocksize=1,
      par=par, fn=fn, gr=gr, ..., method=method,
      maxiter=maxiter, maxeval=maxeval,
      maxtime=maxtime,
      verbose=verbose,
      track=track
    )
  )
}
