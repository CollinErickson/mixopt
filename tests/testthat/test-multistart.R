library(testthat)

test_that("multistart", {
  f6 <- function(x) {-(-x[[1]]*.5*sin(.5*x[[1]])*1 - 1e-2*x[[2]]^2 +
                         .2*x[[1]] - .3*x[[2]])}
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
  # expect_equal(class(ms6$par), c("mixopt_list", "list"))
  expect_true(is.numeric(ms6$par))

  # Check print
  expect_error(capture.output(print(ms6)), NA)
  expect_true(all(c("par", "val", "track", "counts", "runtime") %in% names(ms6)))


  # Test plot
  expect_error({
    plot_track(ms6)
  }, NA)

})

test_that("optim examples", {
  fr <- function(x) {   ## Rosenbrock Banana function
    x1 <- x[1]
    x2 <- x[2]
    100 * (x2 - x1 * x1)^2 + (1 - x1)^2
  }
  optim(c(-1.2,1), fr)
  expect_no_error(
    mixopt_multistart(
      c(mopar_cts(-5,5),
        mopar_cts(-5,5)),
      fr
    )
  )
})

test_that("func using sum/slice", {
  f <- function(x) {sum(x)}
  expect_no_error({
    mixopt_multistart(
      c(mopar_cts(0,1),
        mopar_cts(0,1)),
      f
    )
  })

  f <- function(x) {cos(sum(sqrt(x)))}
  expect_no_error({
    mixopt_multistart(
      c(mopar_cts(0,1),
        mopar_cts(0,1)),
      f
    )
  })

  f <- function(x) {sum(x[1:2]) - x[3]}
  expect_no_error({
    mixopt_multistart(
      c(mopar_cts(0,1),
        mopar_cts(0,1),
        mopar_cts(0,1)),
      f
    )
  })
})

test_that("groupeval", {
  d <- 3
  pars <- replicate(d, mopar_cts(1,10), simplify = F)
  f1 <- function(x) {sum(x^1.6)}
  fng <- function(x) {
    if (is.matrix(x)) {
      apply(x, 1, f1)
    } else {
      # Sys.sleep(.0005)
      f1(x)
    }
  }
  fng(1:3)
  fng(matrix(1:9, byrow=T, ncol=d))
  expect_no_error(mixopt_multistart(par=pars, fn=fng, groupeval='matrix'))
  expect_no_error(mixopt_multistart(par=pars, fn=fng, groupeval=F))
})
