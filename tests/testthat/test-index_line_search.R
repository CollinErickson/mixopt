library(testthat)

# I shouldn't need this line, but can't find the function without it. Weird.
library(mixopt)

test_that("index_line_search", {
  expect_error(ils1 <- index_line_search(function(x) {(-x-100)^2}, (-51):53, plot="x"), NA)
  expect_equal(ils1$ind, 1)
  expect_equal(ils1$x, -51)
  expect_equal(ils1$val, 49^2)

  expect_error(ils1 <- index_line_search(function(x) {(x)^2}, 0, plot="x"), NA)
  expect_equal(ils1$ind, 1)
  expect_equal(ils1$x, 0)
  expect_equal(ils1$val, 0)

  expect_error(ils1 <- index_line_search(function(x) {(x)^2}, 0:1, plot="x"), NA)
  expect_equal(ils1$ind, 1)
  expect_equal(ils1$x, 0)
  expect_equal(ils1$val, 0)

  expect_error(ils1 <- index_line_search(function(x) {-(x)^2}, 0:1, plot="x"), NA)
  expect_equal(ils1$ind, 2)
  expect_equal(ils1$x, 1)
  expect_equal(ils1$val, -1)

  expect_error(ils1 <- index_line_search(function(x) {(x)^2}, -1:1, plot="x"), NA)
  expect_equal(ils1$ind, 2)
  expect_equal(ils1$x, 0)
  expect_equal(ils1$val, 0)

  # High verbose
  expect_error(capture.output({
    ils1 <- index_line_search(function(x) {(x)^2}, -20:88,
                              plot="ind", verbose = 100)}), NA)
  expect_equal(ils1$ind, 21)
  expect_equal(ils1$x, 0)
  expect_equal(ils1$val, 0)
})

test_that("full_index_line_search", {
  f1 <- function(x) {ifelse(x==0, 100, ifelse(abs(x)==1, 1-x/1e5, 10+abs(x)/1e4))}
  # cbind(-10:10, f1(-10:10))
  expect_error(fils1 <- full_index_line_search(f1, (-10):10, 11), NA)
  expect_equal(fils1$ind, 12)
  expect_equal(fils1$x, 1)
  expect_equal(fils1$val, 1-1/1e5)

  f2 <- function(x) {ifelse(x==0, 100, ifelse(abs(x)==1, 1+x/1e5, 10+abs(x)/1e4))}
  # cbind(-10:10, f2(-10:10))
  expect_error(fils2 <- full_index_line_search(f2, (-10):10, 11), NA)
  expect_equal(fils2$ind, 10)
  expect_equal(fils2$x, -1)
  expect_equal(fils2$val, 1-1/1e5)
})

test_that("start point is best", {
  # This gave error before
  xarray <- c(.1,.2,.3,.4)
  ff <- function(x) {
    ifelse(x < .15, -.71,
           ifelse(x < .25, -.0067,
                  ifelse(x < .35, -2e-16, 0)))
  }
  expect_no_error({
    fils3 <- full_index_line_search(
      f=ff,
      xarray=xarray,
      startind = 1
    )
  })
  expect_equal(fils3$ind, 1)
  expect_equal(fils3$x, .1)
  expect_equal(fils3$val, -.71)
  rm(fils3)
  expect_no_error({
    fils3b <- full_index_line_search(
      f=ff,
      xarray=xarray,
      startind = 1,ystart=ff(xarray[1])
    )
  })
  expect_equal(fils3b$ind, 1)
  expect_equal(fils3b$x, .1)
  expect_equal(fils3b$val, -.71)
  rm(fils3b)


  # This gave error before
  xarray <- c(.1,.2,.3,.4)
  ff <- function(x) {
    ifelse(x >.35, -.71,
           ifelse(x > .25, -.0067,
                  ifelse(x > .15, -2e-16, 0)))
  }
  expect_no_error({
    fils3 <- full_index_line_search(
      f=ff,
      xarray=xarray,
      startind = 4
    )
  })
  expect_equal(fils3$ind, 4)
  expect_equal(fils3$x, .4)
  expect_equal(fils3$val, -.71)
  rm(fils3)
})
