library(cbccy)
context("data manipulations")

test_that("year is leap year", {
  expect_equal(is.leapYear(2000), TRUE)
  expect_equal(is.leapYear(2004), TRUE)
  expect_equal(is.leapYear(2003), FALSE)
})