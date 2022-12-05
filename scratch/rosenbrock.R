# Optim uses more evals than it says
evals <- 0
fr <- function(x) {   ## Rosenbrock Banana function
  evals <<- evals + 1
  # print(x)
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
grr <- function(x) { ## Gradient of 'fr'
  x1 <- x[1]
  x2 <- x[2]
  c(-400 * x1 * (x2 - x1 * x1) - 2 * (1 - x1),
    200 *      (x2 - x1 * x1))
}
optim(c(-1.2,1), fr)
optim(c(-1.2,1), fr, gr=grr, method = "L-BFGS-B")
optim(c(-1.2,1), fr, method = "L-BFGS-B", lower=c(-5,-5), upper=c(5,5))

ContourFunctions::cf_func(fr, xlim=c(-5,5), ylim=c(-5,5))
# This takes way too many evals
mrb <- mixopt_multistart(
  par=c(mopar_cts(-5,5),
        mopar_cts(-5,5)),
  fn=fr, track = T
)
mrb

# With grad
mrbg <- mixopt_multistart(
  par=c(mopar_cts(-5,5),
        mopar_cts(-5,5)),
  n1=1,
  fn=fr, gr=grr, track = T
)
mrbg
plot_track(mrbg)

# blockcd, should be same as optim
mrcd <- mixopt_blockcd(
  par=c(mopar_cts(-5,5, start=-1.2),
        mopar_cts(-5,5, start=1)),
  fn=fr, track = T
)
mrcd
plot_track(mrcd)
