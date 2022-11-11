#' Plot the tracked parameters from an optimization
#'
#' @param out Output from mixopt
#'
#' @return Plot
#' @export
#'
#' @examples
#' f8 <- function(x) {-(x[[1]]+x[[2]]) + .1*(x[[1]] - x[[2]])^2}
#' ContourFunctions::cf_func(f8, xlim=c(0,100), ylim=c(0,100))
#' m8 <- mixopt_coorddesc(par=list(mopar_ordered(0:100), mopar_ordered(0:100)),
#'                        fn=f8, track = TRUE)
#' plot_track(m8)
#'
#' library(ggplot2)
#' library(dplyr)
#' ContourFunctions::cf_func(f8, xlim=c(0,100), ylim=c(0,100),
#'                           gg = TRUE) +
#'   geom_point(data=as.data.frame(matrix(unlist(m8$track$par),
#'                                 ncol=2, byrow=TRUE)) %>%
#'                     bind_cols(newbest=m8$track$newbest),
#'              aes(V1, V2, color=newbest))
plot_track <- function(out) {
  stopifnot(!is.null(out$track))
  dflist <- list()
  dflist$val <- out$track$val
  dflist$iter <- 1:length(dflist$val)
  dflist$newbest <- out$track$newbest
  # Separate plot for each index, plus 1 for val
  plots <- list()
  for (i in 1:length(out$track$par[[1]])) {
    dflist[[paste0("par", i)]] <- sapply(out$track$par, function(x) {x[[i]]})
    plots[[length(plots) + 1]] <- ggplot2::ggplot(as.data.frame(dflist),
                                                  ggplot2::aes_string("iter", paste0("par", i), color="newbest")) +
      ggplot2::geom_point()
  }
  # Add val
  plots[[length(plots) + 1]] <- ggplot2::ggplot(as.data.frame(dflist),
                                                ggplot2::aes_string("iter", "val", color="newbest")) +
    ggplot2::geom_point()

  # df <- as.data.frame(dflist)
  # dftall <- tidyr::pivot_longer(cols=colnames(df))
  # ggplot2::ggplot(df)
  # gridExtra::grid.arrange(plots)
  do.call(gridExtra::grid.arrange, plots)
}
if (F) {
  pt <- mixopt_coorddesc(par=list(mopar_cts(2,8),
                                  mopar_unordered(letters[1:6]),
                                  mopar_ordered(1:10)),
                         fn=function(x) {ifelse(x[[2]] == 'b', -1, 0) +(4.5-x[[1]])^2 + x[[1]]*(log(x[[3]])-1)^2},
                         track = T)
  plot_track(pt)

  f8 <- function(x) {-(x[[1]]+x[[2]]) + .1*(x[[1]] - x[[2]])^2}
  ContourFunctions::cf_func(f8, xlim=c(0,100), ylim=c(0,100))
  m8 <- mixopt_coorddesc(par=list(mopar_ordered(0:100), mopar_ordered(0:100)),
                         fn=f8, track = TRUE)
  plot_track(m8)
  ContourFunctions::cf_func(f8, xlim=c(0,100), ylim=c(0,100),
                            # pts=matrix(unlist(m8$track$par), ncol=2, byrow=TRUE),
                            gg = TRUE) +
    ggplot2::geom_point(data=as.data.frame(matrix(unlist(m8$track$par), ncol=2, byrow=TRUE)) %>%
                          bind_cols(newbest=m8$track$newbest),
                        ggplot2::aes(V1, V2, color=newbest))

}
