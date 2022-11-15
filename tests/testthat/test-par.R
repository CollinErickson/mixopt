library(testthat)

test_that("verify par", {
  expect_error(verify_par(1))
  expect_error(mopar_cts(1,5), NA)
  expect_error(c(mopar_cts(1,5), mopar_ordered(1:5)), NA)
  expect_error(verify_par(c(mopar_cts(1,5))), NA)
  expect_error(verify_par(list(1,5)))
})

test_that("mopar_cts", {
  p1 <- mopar_cts(-3,3)
  expect_s3_class(p1, "mixopt_par")
  expect_s3_class(p1, "mixopt_par_cts")
  expect_s3_class(p1, "list")
  expect_equal(p1$lower, -3)
  expect_equal(p1$upper, 3)
  expect_equal(p1$start, 0)
  expect_error(mopar_cts(3))
  expect_error(mopar_cts(3:4,3))
  expect_error(mopar_cts(3,3:4))
  expect_error(mopar_cts(3,4, c(3.3,3.4)))
  expect_error(mopar_cts(3,4, 5))
  s1 <- p1$sample(100)
  expect_true(all(s1 >= -3))
  expect_true(all(s1 <= 3))
  expect_equal(length(s1), 100)
  # Check print
  expect_error(cop1 <- capture.output(print(p1)), NA)
  expect_true("character" %in% class(cop1))
  expect_equal(cop1,
               "mopar_cts(lower = -3, upper = 3, start = 0)")
})

test_that("mopar_ordered", {
  p1 <- mopar_ordered(1:10)
  expect_true("mixopt_par" %in% class(p1))
  expect_true("mixopt_par_ordered" %in% class(p1))
  expect_true("list" %in% class(p1))
  expect_equal(p1$values, 1:10)
  expect_equal(p1$start, 5)
  p2 <- mopar_ordered(1:10, start=9)
  expect_equal(p2$values, 1:10)
  expect_equal(p2$start, 9)
  s1 <- p1$sample(8)
  expect_equal(length(s1), 8)
  expect_true(all(s1 %in% p1$values))
  # Error if any duplicates
  expect_error(capture.output(mopar_ordered(c(1,2,3,4,5,5))))
  # Check print
  expect_error(cop1 <- capture.output(print(p1)), NA)
  expect_equal(length(cop1), 3)
  expect_equal(cop1[1], "mopar_ordered with 10 values")
  expect_equal(cop1[2], "\t values = 1 2 3 4 5 6 7 8 9 10")
  # Many values, check print
  expect_error(p100 <- mopar_ordered(1:100), NA)
  expect_error(cop100 <- capture.output(print(p100)), NA)
  expect_equal(length(cop100), 3)
  expect_equal(cop100[1], "mopar_ordered with 100 values")
  expect_equal(cop100[2], "\t values = 1 2 3 4 5 6 7 8 9 10 ... (80 more values) ... 91 92 93 94 95 96 97 98 99 100")
  expect_error(p100$q(runif(10)), NA)
  # Single value
  expect_error(g1 <- mopar_ordered('a'), NA)
  expect_equal(g1$sample(10), rep('a', 10))
  expect_equal(g1$q(runif(10)), rep('a', 10))

  # print for ordered/unordered doesn't work on other objects
  expect_error(mixopt:::print_mixopt_par_ordered_or_unordered(3))
})

test_that("mopar_unordered", {
  expect_error(p1 <- mopar_unordered(1:10), NA)
  expect_true("mixopt_par" %in% class(p1))
  expect_true("mixopt_par_unordered" %in% class(p1))
  expect_true("list" %in% class(p1))
  expect_equal(p1$values, 1:10)
  expect_true(p1$start %in% 1:10)
  s1 <- p1$sample(8)
  expect_equal(length(s1), 8)
  expect_true(all(s1 %in% p1$values))
  # Error if any duplicates
  expect_error(capture.output(mopar_unordered(c(1,2,3,4,5,5))))
  # Check print
  expect_error(cop1 <- capture.output(print(p1)), NA)
  expect_equal(length(cop1), 3)
  expect_equal(cop1[1], "mopar_unordered with 10 values")
  expect_equal(cop1[2], "\t values = 1 2 3 4 5 6 7 8 9 10")
  # Many values, check print
  expect_error(p100 <- mopar_unordered(1:100), NA)
  expect_error(cop100 <- capture.output(print(p100)), NA)
  expect_equal(length(cop100), 3)
  expect_equal(cop100[1], "mopar_unordered with 100 values")
  expect_equal(cop100[2], "\t values = 1 2 3 4 5 6 7 8 9 10 ... (80 more values) ... 91 92 93 94 95 96 97 98 99 100")
  expect_error(p100$q(runif(100)), NA)
  # Single value
  expect_error(g1 <- mopar_unordered('a'), NA)
  expect_equal(g1$sample(10), rep('a', 10))
  expect_equal(g1$q(runif(10)), rep('a', 10))
})

test_that("c on mopar", {
  p1 <- mopar_cts(0, 1)
  p2 <- mopar_unordered(letters)
  expect_error({
    cpar <- c(p1, p2)
  }, NA)
  expect_equal(length(cpar), 2)
  # Check print
  expect_error(capture.output(print(cpar)), NA)
})
