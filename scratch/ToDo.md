To do

* Global optimizer: multistart, GP, genetic/evolution

  * GP: fit. Run EI. Avoid infinite loop.

* README

* Better examples

* constraints?

* tests: (check codecov to see what tests to add)

* coorddesc with cts: repeating same output many times?

* coorddesc: seems to spend a lot of time not improving at end.
Rosenbrock example from optim doc is a disaster in multistart.

* add stopping reason (maxeval, maxiter, maxtime, normal)

* index line search
  - add more prints for verbose

* par: print wraparound for discrete

* check for NA, etc

* Add references, requested by CRAN

* maxeval for multistart, line search

* maxtime for multistart, line search

* make sure par is valid at end

* optim: first eval reads existing value

* option for matrix/df evaluation. Useful for multistart init points. E.g., EI.

* Need to stop earlier: waiting for 0 improvement is too slow, check what optim does

* Add control. Use reltol.

* Clean up mixopt()

* multistart local arg should be used

* mixopt_list_matrix, each row is a mixopt_list, columns are same type

* as.data.frame works differently on mixopt_list vs after simplified to numeric?
Need to add class after simplifying so it works properly?
