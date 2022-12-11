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
