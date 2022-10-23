get_track_focus_index <- function(trk) {
  n <- length(trk$val)
  d <- length(trk$par[[1]])
  focus_index <- rep(NA, n)
  if (n >= 2) {
    for (i in 2:n) {
      for (j in 1:d) {
        if (abs(trk$par[[i]][[j]] - trk$par[[i-1]][[j]]) > 0) {
          # This index is change
          # Multiple focus index happens all the time for random starts
          # if (!is.na(focus_index[i])) {
          #   warning(paste0("Error: Multiple focus index"))
          # }
          focus_index[[i]] <- j
        }
      }
    }
  }
  focus_index
}
