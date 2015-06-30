library(cbccy)
context("data IO and manipulations")

#change to package root
setwd("../../")

#variables
data.dir = "data/locations/"

#################
#readBinaryData()
test_that("read binary data and output correct length", {
  expect_equal(length(readBinaryData("data/locations/data_46.71875_-117.21875")), 134412)
})
test_that("data output is integer", {
  expect_equal(typeof(readBinaryData("data/locations/data_46.71875_-117.21875")), "integer")
})

#################
#getHistoryData()
test_that("years are those selected", {
  expect_equal(getHistoryData("46.71875", "-117.21875", 1970, 2000)$date[1],as.Date("1970-01-01"))
  expect_equal(getHistoryData("46.71875", "-117.21875", 1970, 2000)$date[11323],as.Date("2000-12-31"))
})

######
#getClimateData()
test_that("data values are factored and correct", {
  expect_equal(getClimateData("41.28125", "-116.21875")$precipitation[1],0)
  expect_equal(getClimateData("41.28125", "-116.21875")$temp.max[1],4.73)
  expect_equal(getClimateData("41.28125", "-116.21875")$temp.min[1],-15.47)
  expect_equal(getClimateData("41.28125", "-116.21875")$windspeed[1],2.92)
  
})





