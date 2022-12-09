library(testthat)

test_that("coorddesc cts 1D list", {
  expect_error(
    {
      cdout <- mixopt_coorddesc(par=list(mopar_cts(2,8)),
                                fn=function(x) {(4.5-x[1])^2})
    }, NA
  )
  expect_equal(length(cdout$par), 1)
  expect_equal(cdout$par[[1]], 4.5)
  expect_equal(cdout$val, 0)
})

test_that("coorddesc cts 1D no list", {
  expect_error(
    {
      cdout <- mixopt_coorddesc(par=mopar_cts(2,8),
                                fn=function(x) {(4.5-x[1])^2})
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


test_that("coorddesc cts and un", {
  expect_error(
    {
      capture.output({
        cdout <- mixopt_coorddesc(par=list(mopar_cts(5,15),
                                           mopar_unordered(2:8, start=2)),
                                  fn=function(x) {
                                    (x[[1]] - 11.1) ^ 2 * (x[[2]] - 5.4) ^ 2 +
                                      abs(x[[1]] - 11.1) + abs(x[[2]] - 5.4)
                                  }, verbose=1e5)
      })
    }, NA
  )
  expect_equal(length(cdout$par), 2)
  expect_equal(cdout$par[[1]], 11.1)
  expect_equal(cdout$par[[2]], 5)
  expect_equal(cdout$val, .4)
})


test_that("cts, ord, un", {
  expect_error(
    {
      capture.output({
        cdout <- mixopt_coorddesc(par=list(mopar_cts(5,15, start=8.4),
                                           mopar_ordered(5:11, start=11),
                                           mopar_unordered(2:8, start=2)),
                                  fn=function(x) {
                                    (x[[1]] - 11.1) ^ 2 * (x[[2]] - 5.4) ^ 2 +
                                      abs(x[[1]] - 11.1) + abs(x[[2]] - 5.4) +
                                      x[3]^2
                                  }, verbose=1e5, track=T)
      })
    }, NA
  )
  expect_equal(length(cdout$par), 3)
  expect_equal(cdout$par[[1]], 11.1)
  expect_equal(cdout$par[[3]], 2)
  # expect_equal(cdout$val, .4)
  # Make sure start is being used
  expect_equal(unlist(cdout$track$par[[1]]), c(8.4, 11, 2))
})

test_that("stopifnot", {
  expect_error({
    cdout <- mixopt_coorddesc(par=list(mopar_cts(2,8)),
                              fn=function(x) {(4.5-x[[1]])^2},
                              maxeval = .3)
  })
})
