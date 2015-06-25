library(cbccy)
context("helper functions")

###############
# is.leapYear()
test_that("year is leap year", {
  expect_equal(is.leapYear(2000), TRUE)
  expect_equal(is.leapYear(2004), TRUE)
})

test_that("year is not a leap year", {
  expect_equal(is.leapYear(2003), FALSE)
})

#########
# dateVector()
test_that("return is correct length", {
  expect_equal(length(dateVector(1915,2006)), 33603)
})
test_that("leap year has correct number of days", {
  expect_equal(length(dateVector(2004,2004)), 366)
})
test_that("first day is jan 1", {
  expect_equal(as.character(dateVector(1915,1915)[1]),"1915-01-01")
})
test_that("last day is december 31", {
  expect_equal(as.character(dateVector(1915,1915)[365]),"1915-12-31")
}) 
  
  
  
  
  
  
  