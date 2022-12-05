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

* Use numeric instead of mixopt_list when all are numeric

* option for matrix/df evaluation. Useful for multistart init points. E.g., EI.

* Need to stop earlier: waiting for 0 improvement is too slow, check what optim does

* Replace coorddesc with blockcd with maxblocksize=1 to avoid repetitive code

* Add control. Use reltol.

* To have math funcs work on mixopt_lists, convert them to numeric when all
numeric? Can also do that when subsetting if it removes non-numeric.

* Fix bad full index line search when start is best.
