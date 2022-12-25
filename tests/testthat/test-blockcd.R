library(testthat)

test_that("blockcd all types", {
  f <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    d <- x[4]
    f <- x[5]
    a+a*b+b^2 +5*(d-.22)^2*(f-22)^2
  }
  mop <- mop <- c(
    mixopt::mopar_cts(0,1),
    mixopt::mopar_cts(-1,1),
    mixopt::mopar_unordered(5:6),
    mixopt::mopar_ordered(c(.1,.2,.3,.4)),
    mixopt::mopar_ordered(10:30)
  )
  expect_no_error({
    mixopt_blockcd(
      par=mop, fn=f
    )
  })
})


test_that("blockcd ord and unord", {
  f <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    d <- x[4]
    f <- x[5]
    a+a*b+b^2 +5*(d-.22)^2*(f-22)^2
  }
  mop <- mop <- c(
    mixopt::mopar_unordered(0:1),
    mixopt::mopar_ordered(1:3),
    mixopt::mopar_unordered(5:6),
    mixopt::mopar_ordered(c(.1,.2,.3,.4)),
    mixopt::mopar_ordered(10:30)
  )
  expect_no_error({
    mixopt_blockcd(
      par=mop, fn=f
    )
  })
})

test_that("fngr", {
  d <- 3000
  fn1 <- function(x) {mean(x^1.34 * log(x) + 1/(x))}
  # curve(sapply(x, fn), 0, 10)
  gr1 <- function(x) {(1.34*x^.34 * log(x) + x^1.34 / x -x^-2) / length(x)}
  # curve(gr, 1, 10, lwd=5)
  # curve(sapply(x, function(x) {numDeriv::grad(fn, x)}), add=T, col=2, lwd=2)
  # numDeriv::grad(fn, 1:10)
  # gr(1:10)

  fngr1 <- function(x) {
    list(fn=mean(x^1.34 * log(x) + 1/(x)),
         gr=(1.34*x^.34 * log(x) + x^1.34 / x -x^-2) / length(x)
    )
  }
  # curve(sapply(x, fn), 0, 10)

  parl <- list()
  for (i in 1:d) {
    parl[[i]] <- mopar_cts(.1,1000000)
  }
  # mixopt_blockcd(parl, fn=fn)
  expect_no_error(mixopt_blockcd(parl, fn=fn1, gr=gr1))
  expect_no_error(mixopt_blockcd(parl, fn=fn1, fngr=fngr1, maxblocksize=1000))
  expect_no_error(mixopt_blockcd(parl, fngr=fngr1))
  # Error if neither fn/fngr
  expect_error(mixopt_blockcd(parl, gr=gr1))
})

test_that("fngr2, smaller dim", {
  d <- 10
  fn1 <- function(x) {mean(x^1.34 * log(x) + 1/(x))}
  gr1 <- function(x) {(1.34*x^.34 * log(x) + x^1.34 / x -x^-2) / length(x)}
  fngr1 <- function(x) {
    list(fn=mean(x^1.34 * log(x) + 1/(x)),
         gr=(1.34*x^.34 * log(x) + x^1.34 / x -x^-2) / length(x)
    )
  }

  parl <- list()
  for (i in 1:d) {
    if (i %% 3 == 0) {
      parl[[i]] <- mopar_cts(.1,1000000)
    } else if (i %% 3 == 1) {
      parl[[i]] <- mopar_ordered(seq(.1,1000000,l=1e4))
    } else {
      parl[[i]] <- mopar_unordered(seq(.1,1000000,l=1e4))
    }
  }
  # mixopt_blockcd(parl, fn=fn)
  expect_no_error(capture.output(mixopt_blockcd(parl, fn=fn1, gr=gr1,
                                                verbose=1e9, track=T)))
  expect_no_error(capture.output(mixopt_blockcd(parl, fn=fn1, fngr=fngr1,
                                                verbose=1e9, track=T)))
  expect_no_error(capture.output(mixopt_blockcd(parl, fngr=fngr1, verbose=1e9)))
})


# Errors ----
test_that("errors", {
  # Bad reltol
  expect_error(mixopt_blockcd(par=mopar_cts(1,4), fn=function(x) {x},
                              control=list(reltol=1:5)))
  # Neither fn or fngr
  expect_error(mixopt_blockcd(par=mopar_cts(1,4)))
})
