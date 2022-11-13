To do

* Global optimizer: multistart, GP, genetic/evolution

  * GP: fit. Run EI. Avoid infinite loop.

* README

* Better examples

* Boolean, integer?

* Block coorddesc (all cts at once)

* constraints?

* tests: (check codecov to see what tests to add)

* coorddesc with cts: repeating same output many times?

* coorddesc: seems to spend a lot of time not improving at end

* set max run time

* add stopping reason (maxeval, maxiter, maxtime, normal)

* index line search

  - add more prints for verbose

* par: print wraparound for discrete

* check for NA, etc

* Allow for sequences/integers, so all values don't have to be given.
E.g. 1:1e7 may be too big to store in memory.

* Add references, requested by CRAN

* maxeval for multistart, line search

* maxtime for multistart, line search

* verify_par should return TRUE instead of NULL

* Fix 3.5 in indexlinesearchfunction

* wrong: index_line_search(function(x) {(-x-100)^2}, (-51):53, plot="x")

* make sure par is valid at end
