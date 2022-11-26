library(testthat)

test_that("multistart", {
  f6 <- function(x) {-(-x[[1]]*.5*sin(.5*x[[1]])*1 - 1e-2*x[[2]]^2 + .2*x[[1]] - .3*x[[2]])}
  # Make sure multistart runs
  expect_error({
    ms6 <- mixopt_multistart(par=list(mopar_cts(0,100), mopar_cts(-100,100)),
                             fn=f6, track = TRUE)
  }, NA)
  # Check output
  expect_true("mixopt_output_list" %in% class(ms6))
  expect_true("list" %in% class(ms6))
  expect_equal(length(ms6$val), 1)
  expect_true(is.numeric(ms6$val))
  expect_equal(length(ms6$par), 2)
  expect_equal(class(ms6$par), c("mixopt_list", "list"))

  # Check print
  expect_error(capture.output(print(ms6)), NA)
  expect_true(all(c("par", "val", "track", "counts", "runtime") %in% names(ms6)))


  # Test plot
  expect_error({
    plot_track(ms6)
  }, NA)

})
