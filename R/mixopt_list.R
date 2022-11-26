#' Title
#'
#' @param x x
#' @param i i
#' @param value value
#'
#' @return value
#' @export
#'
#' @examples
#' a <- list(1,4,'c')
#' class(a) <- "mixopt_list"
#' a[3]
`[.mixopt_list` <- function(x, i, value) {
  # print("using new")
  # print(x)
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
# print.mixopt_list <-
