library(testthat)

test_that("coorddesc cts", {
  expect_error(
    {
      cdout <- mixopt_coorddesc(par=list(mopar_cts(2,8)),
                                fn=function(x) {(4.5-x[[1]])^2})
    }, NA
  )
  expect_equal(length(cdout$par), 1)
  expect_equal(cdout$par[[1]], 4.5)
  expect_equal(cdout$val, 0)
})

test_that("coorddesc ordered", {
  expect_error(
    {
      cdout <- mixopt_coorddesc(par=list(mopar_ordered(2:89)),
                                fn=function(x) {(4.6-x[[1]])^2})
    }, NA
  )
  expect_equal(length(cdout$par), 1)
  expect_equal(cdout$par[[1]], 5)
  expect_equal(cdout$val, .16)

  # Smaller search
  expect_error(
    {
      cdout <- mixopt_coorddesc(par=list(mopar_ordered(2:3)),
                                fn=function(x) {(4.6-x[[1]])^2})
    }, NA
  )
  expect_equal(cdout$par[[1]], 3)
  expect_equal(cdout$val, 1.6^2)

  # Reverse direction
  expect_error(
    {
      cdout <- mixopt_coorddesc(par=list(mopar_ordered(2:89, start = 89)),
                                fn=function(x) {(4.6-x[[1]])^2})
    }, NA
  )
  expect_equal(length(cdout$par), 1)
  expect_equal(cdout$par[[1]], 5)
  expect_equal(cdout$val, .16)

  # Start in middle
  expect_error(
    {
      cdout <- mixopt_coorddesc(par=list(mopar_ordered(2:89, start = 45)),
                                fn=function(x) {(4.6-x[[1]])^2})
    }, NA
  )
  expect_equal(length(cdout$par), 1)
  expect_equal(cdout$par[[1]], 5)
  expect_equal(cdout$val, .16)
})

test_that("coorddesc unordered", {
  expect_error(
    {
      cdout <- mixopt_coorddesc(par=list(mopar_unordered(2:8, start=8)),
                                fn=function(x) {(4.6-x[[1]])^2})
    }, NA
  )
  expect_equal(length(cdout$par), 1)
  expect_equal(cdout$par[[1]], 5)
  expect_equal(cdout$val, .16)
})
