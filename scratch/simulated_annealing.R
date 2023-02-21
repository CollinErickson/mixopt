# 2/12/23
# Started to try to implement simulated annealing.
# But it's not clear how to choose starting temp.
# Seems like very inefficient alg.
# Maybe not worth implementing.
# Some discussion here how to auto pick starting temp:
#  https://www.jontse.com/courses/files/cornell/cs5722/11-09-03_SimulatedAnnealing.pdf
# Copied alg from here:
#  https://en.wikipedia.org/wiki/Simulated_annealing#:~:text=Simulated%20annealing%20(SA)%20is%20a,space%20for%20an%20optimization%20problem.


E <- function(state) {

}
temperature <- function(prop) {

}
neighbor <- function(state) {

}
acceptanceprob <- function(E, Enew, temp) {
  if (Enew < E) {
    1
  } else {
    exp(-(E - Enew) / temp)
  }
}

s0 <- c(mopar_cts(2, 8), mopar_ordered(letters)) # start point
maxiter <- 10
i <- 0
s <- s0
Es <- E(s)
# loop
temp <- temperature(1 - (i+1)/maxiter)
snew <- neighbor(s)
Esnew <- E(snew)
Pnew <- acceptanceprob(Es, Esnew, temp)
if (Pnew >= runif(1)) {
  s <- snew
  Es <- Esnew
}
