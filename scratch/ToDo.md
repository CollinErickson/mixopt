To do

* Global optimizer: multistart, GP, genetic/evolution

  * GP: fit. Run EI. Avoid infinite loop.

* README

* Better examples

* Block coorddesc (all cts at once)

* constraints?

* tests: (check codecov to see what tests to add)

* coorddesc with cts: repeating same output many times?

* coorddesc: seems to spend a lot of time not improving at end.
Rosenbrock example from optim doc is a disaster in multistart.

* set max run time

* add stopping reason (maxeval, maxiter, maxtime, normal)

* index line search
  - add more prints for verbose

* par: print wraparound for discrete

* check for NA, etc

* Add references, requested by CRAN

* maxeval for multistart, line search

* maxtime for multistart, line search

* make sure par is valid at end

* If single par, allow input as just mopar, not list.

* optim: first eval reads existing value

* Option for multipredict. Use for multistart first stage.

* Use grad for cts

* Use fngr

* ms and cd need to use mopar$start when they have it
