library(cbccy)
context("test calculations")

#change to package root
setwd("../../")

testdf = data.frame(
  date = as.Date(c("2010-06-01", "2010-06-02", "2011-07-01", "2011-07-02", 
                   "2012-08-20", "2012-08-21", "2012-08-22")),
  temp.hi = c(50,55,70,80,86,90,100),
  temp.low = c(-50,40,60,65,50,86,100)
)

#                    temp.base=50, 
#                    restrict=25, 
#                    temp.max=86, 
#                    temp.min=50) {
  

##########
#calcGDD()
# calculation is (temp.hi-temp.low)/2 - base
test_that("gdd calculation is correct.", {
  expect_equal(calcGDD(testdf$temp.hi, testdf$temp.low), c(0.0,2.5,15.0,22.5,18.0,25.0,25.0))
})
test_that("variables can be changed.", {
  expect_equal(calcGDD(testdf$temp.hi, testdf$temp.low, temp.base=10), c(25.0,25.0,25.0,25.0,25.0,25.0,25.0))
})

#############
#calcCumGDD()
test_that("", {
  expect_equal(calcCumGDD(testdf$date, testdf$temp.hi, testdf$temp.low))
})







