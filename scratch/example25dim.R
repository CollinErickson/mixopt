# Example from optim doc, 25 inputs
evals <- 0
flb <- function(x){
  evals <<- evals + 1
  p <- length(x)
  sum(c(1, rep(4, p-1)) * (x - c(1, x[-p])^2)^2)
}
## 25-dimensional box constrained
optim(rep(3, 25), flb, NULL, method = "L-BFGS-B",
      lower = rep(2, 25), upper = rep(4, 25)) # par[24] is *not* at boundary
print(evals)
par25 <- list()
for (i in 1:25) {
  par25[[i]] <- mopar_cts(2,4)
}
mixopt_blockcd(par = par25, fn = flb)
mixopt_blockcd(par = par25, fn = flb, maxblocksize = 6, track = T)
