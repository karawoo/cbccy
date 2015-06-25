#' @title cbccy-package: Tools and analysis for the impact of climate change on the Columbia River Basin
#'
#' @description This package contains the code and various implementations of decision-tools used to evaluate the impact of climate change on crop yields
#'
#' @author Nicholas Potter
#' @docType package
#' @name cbccy
#' @aliases cbccy
#' @keywords package cbccy-package
#' @examples
#' \dontrun{get.user.repositories("potterzot")}
#' @seealso \code{jsonlite}
#' @seealso \url{http://quickstats.nass.usda.gov/api}
NULL

genTemperature <- function(mean=60, sd=35, factor=4000) {
  x = mean + sd*sin(seq(-2.2, 4, .017)) #temperature as sine wave!
  jitter(x, factor=factor)
}

genPrecipitation <- function(mean=50, sd=10, factor=4000) {
  #two rainy seasons cause why not? 
  x = mean + sd*sin(seq(-6.28, 6.28, 0.0345))
  jitter(x, factor=factor)
}

genWindSpeed <- function(mean=10, sd=10, factor=4000) {
  x = mean + sd*sin(seq(-2.2, 4, .017)) #temperature as sine wave!
  jitter(x, factor=factor)
}

genFakeData <- function(origin_seed=235897, 
                        kind=NULL, 
                        normal.kind=NULL,...) {
  # generate a set of random seeds for different data
  set.seed(origin_seed, kind, normal.kind)
  n.seeds = 5
  seeds = as(runif(n.seeds,0,1000000), "integer")
  
  #Needed data include:
  # avg temp
  # daily high
  # daily low
  # wind speed
  # precipitation
  
  #First create a set of year, month, day records for 4 years (to include a leap year example)
  year = 2012:2016
  month = 1:12
  
  df = data.frame(year, month, day)

  #temperatures
  df['temp.high'] = genTemperature(mean=60)
  df['temp.low'] = genTemperature(mean=35)
  #df['temp.avg'] = 
  
  #precipitation 
  df['precipitation.avg'] = genPrecipitation()
  
  #wind speed
  df['windspeed.avg'] = genWindSpeed()
  df
}




