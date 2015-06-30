
#' Get available locations.
#' 
#' Look at data files to determine available lat/lng combinations.
#' 
#' @export
#' @param dir a directory location, default 'data/locations'.
#' @param write boolean to write a list of all locations, default FALSE
#' @param write.file string filename to write, default "data/locations.csv"
#' @return data frame of latitude/longitude pairs
availableLocations <- function(dir="data/locations", 
                               write=FALSE,
                               write.file = "data/locations.csv") {
  files = list.files(dir)
  m = matrix(nrow=length(files),ncol=2)
  for (i in 1:length(files)) {
    f = strsplit(files[i],"_")[[1]]
    m[i,1] = f[2]
    m[i,2] = f[3]
  }
  locs = data.frame(latitude=m[,1], longitude=m[,2])
  
  if (write) write.csv(locs, file=write.file)
  locs
}





#' Get available crops.
#' 
#' Look at data files to determine what crop data are available.
#' 
#' @export
#' @param dir a directory location, default 'data/locations'.
#' @return list of crop
availableCrops <- function(dir="data/crops") {
  files = list.files(dir)
  m = matrix(nrow=length(files),ncol=2)
  for (i in 1:length(files)) {
    f = strsplit(files[i],"_")[[1]]
    m[i,1] = f[2]
    m[i,2] = f[3]
  }
  data.frame(latitude=m[,1], longitude=m[,2])
}



#' Tests if year is a leap year.
#' 
#' Check to see that year is a leap year
#' 
#' @export
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
#' @export
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






