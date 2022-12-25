library(testthat)

test_that("mixopt_list works", {
  expect_no_error({
    a <- list(1,4,'c', 'g')
    class(a) <- "mixopt_list"
    b <- as.mixopt_list(c(1,2,3,4,5))
  })
  expect_no_error({
    a
    a[3]
    a[2:3]
    a[-(2:3)]
    as.data.frame(a)
    print(a)

    b[3:4]
    b[-(3:4)]
    sum(b)
    b^2
    b+b
    b-b
    b*b
    b/b
    c(b)
    c(b, b)
    c(b, 1)
    c(1, b)
    c(a, b, a)
    c_mixopt_list(0, 1, 2, 3, 4, a, 5, 6, 7, 8, b, 9)
    c_mixopt_list(NULL, 3, NULL, a, NULL, 66666, NULL, b)
    print(b)
    print(c(a, b))
  })
  expect_error(a[c(2, -3)])
  expect_null(c_mixopt_list(NULL))
  expect_equal(a, c_mixopt_list(a))
})

test_that("Converts num/char back", {
  expect_no_error(d <- as.mixopt_list(letters, T))
  expect_equal(class(d), "character")
  expect_no_error(d <- as.mixopt_list(as.list(letters), T))
  expect_equal(class(d), "character")
})
