#' Calculate growing degree days.
#' 
#' Calculates Growing Degree Days from temperature data.
#' 
#' @param temp.hi a vector of daily high temperatures
#' @param temp.low a vector of daily low temperatures
#' @param base a vector of the crop-dependent base temperature
#' @param temp.max a number denoting maximum temperature (default=86)
#' @param temp.min a number denoting minimum temperature (default=50)
#' @return a vector of (noncumulative) GDD
calcGDD <- function(temp.hi, 
                    temp.low, 
                    temp.base=50, 
                    restrict=25, 
                    temp.max=86, 
                    temp.min=50) {
 
  #no temps higher than max or lower than min 
  temp.hi[temp.hi>temp.max] = temp.max
  temp.low[temp.low<temp.min] = temp.min
  
  #can't be lower than 0 or higher than restrict
  pmax(pmin((temp.hi+temp.low)/2-temp.base, 25),0)
}

#' calculate cumulative GDD
#' 
#' calculates cumulative Growing Degree Days
#' 
#' @param dates a vector of formatted dates
#' @param temp.hi a vector of daily high temperatures
#' @param temp.low a vector of daily low temperatures
#' @param base a vector of the crop-dependent base temperature
#' @return a vector of cumulative GDD
calcCumGDD <- function(dates, 
                       temp.hi, 
                       temp.low, 
                       base, 
                       ...) {
  year = as.integer(format(dates, "%Y"))
  gdd = calcGDD(temp.hi, temp.low, base, ...)
  df = data.frame(dates, year, gdd)
  df.by_year = group_by(df, year)
  by.year = summarize(df.by_year, gdd.cum = cumsum(gdd))
}