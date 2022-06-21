#' Plot the traked parameters from an optimization
#'
#' @param out Output from mixopt
#'
#' @return Plot
#' @export
#'
#' @examples
#' 1
plot_track_par <- function(out) {
  browser()
  stopifnot(!is.null(out$track_par))
  dflist <- list()
  dflist$val <- out$track_val
  dflist$iter <- 1:length(dflist$val)
  plots <- list()
  for (i in 1:length(out$track_par[[1]])) {
    dflist[[paste0("par", i)]] <- sapply(out$track_par, function(x) {x[[i]]})
    plots[[length(plots) + 1]] <- ggplot2::ggplot(as.data.frame(dflist),
                                                  ggplot2::aes_string("iter", paste0("par", i))) +
      ggplot2::geom_point()
  }
  # Add val
  plots[[length(plots) + 1]] <- ggplot2::ggplot(as.data.frame(dflist),
                                                ggplot2::aes_string("iter", "val")) +
    ggplot2::geom_point()

  # str(dflist)
  df <- as.data.frame(dflist)
  browser()
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
                         track_par = T)
  plot_track_par(pt)
}
