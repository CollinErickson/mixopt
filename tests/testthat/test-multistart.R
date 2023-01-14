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

test_that("groupeval matrix", {
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
test_that("groupeval df", {
  # Mixed num/char cols
  f <- function(x) {
    if (is.data.frame(x)) {
      colnames(x) <- c('a', 'b')
      x$a^2 + ifelse(x$b=='a', 1, 0)
    } else {
      x[1]^2 + ifelse(x[2]=='a', 1, 0)
    }
  }
  pars <- c(mopar_cts(-2,2), mopar_unordered(letters[1:4]))
  expect_no_error(
    mixopt_multistart(par=pars,
                      fn=f, groupeval="data.frame")
  )

  # All num
  f <- function(x) {
    if (is.data.frame(x)) {
      colnames(x) <- c('a', 'b')
      x$a^2 + x$b
    } else {
      x[1]^2 + x[2]
    }
  }
  pars <- c(mopar_cts(-2,2), mopar_cts(3,11))
  expect_no_error(
    mixopt_multistart(par=pars,
                      fn=f, groupeval="data.frame")
  )
})

test_that("fngr", {
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
  expect_no_error(capture.output(mixopt_multistart(parl, fn=fn1, gr=gr1,
                                                verbose=1e9, track=T)))
  expect_no_error(capture.output(mixopt_multistart(parl, fn=fn1, fngr=fngr1,
                                                verbose=1e9, track=T)))
  expect_no_error(capture.output(mixopt_multistart(parl, fngr=fngr1, verbose=1e9)))
})
