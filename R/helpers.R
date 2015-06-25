

#' Tests if year is a leap year.
#' 
#' Check to see that year is a leap year
#' 
#' @param year an integer
#' @return boolean
#' @examples 
#' is.leapYear(2000) #TRUE
#' is.leapYear(2003) #FALSE
is.leapYear <- function(yr) {
  as.logical(!(yr %% 400) || !((yr %% 4) && (yr %% 100)))
}

#' Create a vector of dates.
#' 
#' Creates a vector of dates from a start and end year, inclusive.
#' 
#' @param year.start the year to start with, default 1915
#' @param year.end the last year, default 2006
#' @return vector of date-formatted values
dateVector <- function(year.start=1915, year.end=2006) {
  day0 = as.Date(paste0(year.start,"-01-01"))
  till.leapyear = abs(2000-year.start) %% 4
  num.years = year.end - year.start - till.leapyear
  
  # 365*number of years until the next leap year +
  # 366 for the first leap year + 
  # floor(365.25*number of years left)
  # - 1
  num.days = 365 * till.leapyear + 366 + floor(365.25*num.years) - 1
  
  #return formatted as dates
  as.Date(day0 + 0:num.days)
}






