# data.R functions for working with climate data


#' Average temperatures over multiple years.
#' 
#' Convert temperature data for multiple years into a single average,
#' with accompanying measures like standard deviation, percentages, etc...
#' 
#' @param df a data frame of climate data for multiple years.
#' @return a dataframe consisting of a single year's worth of average data.
#' @importFrom dplyr group_by summarize ungroup
#' @export
averageTemperatures <- function(df) {
  df.by_day = group_by(df, monthday)
  by_day = summarize(df.by_day, 
            gdd.mean = mean(gdd),
            gdd.sd = sd(gdd),
            gdd.median = median(gdd),
            gdd.max = max(gdd),
            gdd.min = min(gdd),
            gdd.90pct = quantile(gdd, probs=c(.9)),
            gdd.75pct = quantile(gdd, probs=c(.75)),
            gdd.25pct = quantile(gdd, probs=c(.25)),
            gdd.10pct = quantile(gdd, probs=c(.1))) 
  ungroup(by_day) 
 }



  #calculated measures
#  if (is.null(this.year)) this.year = as.integer(format(today, "%Y"))
  
  #Forecast is from forecast date forward
#  if (is.null(forecast.date)) forecast.date = today
#  df$forecast = 0
#  df[df$date>=forecast.date, 'forecast'] = 1
  
#' Select and merge year data with day ranges.
#' 
#' Select years from year data and merge with day ranges.
#' 
#' @param yeardata a df of climate data for range of years.
#' @param byday a df of climate data by day.
#' @param select.years list of years to plot as comparison.
#' @return a dataframe containing all data necessary to plot a GDD chart.
#' @importFrom dplyr filter arrange
#' @export
selectAndMergeYearData <- function(yeardata, 
                            byday, 
                            select.years) {
  
  arrange(merge(by.day, filter(df, year==this.year), by="monthday"), date)
  
}


#' Create daily range climate data.
#'
#' Create a data frame with max/min temp, precip, windspeed, gdd, etc....
#' 
#' @param df a data frame of climate data spanning multiple years.
#' @return a data frame containing growing degree days.
#' @importFrom dplyr filter group_by summarize ungroup
#' @export
createDailyData <- function(df) {
  
  #monthday variable for grouping
  df$monthday = format(df$date, "%m-%d")
 
  # day dimension
  df.by_day = group_by(df, monthday)
  by.day = summarize(df.by_day, 
                     temp.max=max(temp.high),
                     temp.avg=mean((temp.high+temp.low)/2),
                     temp.min=min(temp.low),
                     precip.max = max(precipitation),
                     precip.avg = mean(precipitation),
                     precip.min = min(precipitation),
                     windspeed.max = max(windspeed),
                     windspeed.avg = mean(windspeed),
                     windspeed.min = min(windspeed),
                     #firstfrost.earliest,
                     #firstfrost.latest,
                     #lastfrost.earliest,
                     #astfrost.latest,
                     gdd.avg = mean(gdd),
                     gdd.max = max(gdd),
                     gdd.min = min(gdd),
                     gdd.sd = sd(gdd))
  
  #merge to this year's data by day number
  ungroup(by.day)
}


# year dimension
#df.by_year = group_by(df., year)
#df.by_year = mutate(df.by_year, day.n=seq_along(dates))
#by.year = summarize(df.by_year,
#                    gdd.cum = cumsum(gdd),
#                    gdd.avg.cum = cumsum(gdd.avg))
#merge(by.day, by.year, by=dates)
  
#  gdd$date = as.Date(strptime(paste0(gdd$year,"-",gdd$month,"-",gdd$day), format="%Y-%m-%d"))
#  gdd$monthDay = as.Date(strptime(paste0(gdd$month,"-",gdd$day), format="%m-%d"))
#  return(gdd)


#' Get chart data for a location.
#' 
#' Load climate data and calculate values to provide chart data.
#' 
#' @param year.start the first year of data to include
#' @param year.end the last year of data to include
#' @return data frame of data for graphic 
#' @export
getData <- function(year.start, year.end) {
  
}

#' Load climate data.
#' 
#' Loads climate data for a specific latitude/longitude pair.
#' 
#' @param lat a latitude
#' @param lng a longitude
#' @param data.dir a directory location, default "data/historical/"
#' @param file.name.root filename roots, default "data_"
#' @param column.names data frame column names, default 
#'   c("precipitation", "temp.max", "temp.min", "windspeed")
#' @param column.factors a vector of values to multiply by
#' @return a data frame of climate data
#' @export
readClimateData <- function(lat, 
                             lng, 
                             data.dir="data/historical/", 
                             file.name.root = "data_",
                             column.names = c("precipitation", 
                                              "temp.high", 
                                              "temp.low", 
                                              "windspeed"),
                             column.factors = c(0.025, .01, .01, .01)
                             ) {
  #load the data
  file.name = paste0(data.dir,file.name.root,lat,"_",lng)
  climateVector <- readBinaryData(file.name)
    
  #parse to data frame
  column.num = length(column.names)
  row.num = length(climateVector)/column.num
  df = data.frame(matrix(climateVector, nrow=row.num, ncol=column.num, byrow=TRUE))
  names(df) <- column.names
  
  #factors
  for (i in 1:column.num) {
    df[,column.names[i]] = df[,column.names[i]] * column.factors[i]
  }
  df
}

#' Write climate data to a binary file for each location.
#' 
#' Writes a climate data frame to a file in binary format.
#' 
#' @param df the data frame to write.
#' @param file.name the file name to write to.
#' @param column.names the names of columns to write.
#' @param column.factors the factors to divide by before writing.
#' @return TRUE if successful.
#' @export
writeClimateData <- function(df,
                             file.name,
                             column.names = c("precipitation", 
                                              "temp.high", 
                                              "temp.low", 
                                              "windspeed"),
                             column.factors = c(0.025, .01, .01, .01),
                             ...
                             ) {
  for (i in 1:length(column.factors)) {
    df[column.names[i]] = df[column.names[i]] / column.factors[i]
  }
  
  writeBinaryData(df[,column.names], file.name, ...)
} 

#' Reads binary climate data.
#'
#' Reads binary climate data as a single vector.
#'  
#' @param fname the name of the file to be read
#' @param what the data type to read as, default integer
#' @param size the byte size, default 2
#' @param encoding the binary encoding, default ASCII
#' @param n the number of entries, default 134412 (92 years) 
#' @return vector with read values
#' @export
readBinaryData <- function(file.name, 
                           what="integer", 
                           size=2, 
                           n=134412, 
                           encoding="ASCII", ...) {
  
  to.read = file(file.name, 'rb', encoding=encoding)
  x = readBin(to.read, what=what, size=size, n=n, ...)
  close(to.read)
  x
}

#' Write a data frame to a binary file.
#' 
#' Writes a data frame to binary, with no headers or row names.
#' The written file is intended to mimic the binary data produced
#' by the REACH project.
#' 
#' @param df data frame to be written.
#' @param file.name the file to write to.
#' @param encoding default is ASCII.
#' @return TRUE is successful.
#' @export
writeBinaryData <- function(df, 
                            file.name,
                            size=2,
                            encoding="ASCII") {
  to.write = file(file.name, 'wb', encoding="ASCII")
  writeBin(as.integer(as.vector(as.matrix(df))), to.write, size=size)
  close(to.write)
  unlink(to.write)
}


#' Get climate data for a location.
#' 
#' Get climate data for a location. Wraps loadClimateData()
#' in order to return a data frame with all necessary components
#' for easy plotting.
#' 
#' @param lat the latitude of a location
#' @param lng the longitude of a location
#' @param mode one of 'historical', 'future', or 'present'. 
#'   Determines data to fetch.
#' @param crop a crop object containing gdd information
#' @param data.dir the directory containing 'historical', 'future', 
#'   and 'present' directories.
#' @param ... other parameters passed to loadClimateData()
#' @return a data frame of climate data with dates attached.
#' @export
getLocationData <- function(lat, 
                            lng, 
                            mode,
                            data.dir = "data/",
                            ...) {
  if (mode=='historical') {
    data.dir = paste0(data.dir,"historical/")
    years = c(1915,2006)
  }
  else if (mode=='future') {
    data.dir = paste0(data.dir,"future/")
    years = c(2020,2111)
    
  }
  else if (mode=='present') {
    data.dir = paste0(data.dir,"present/")
    years = c(2015,2015)
  }
  else {
    stop("Mode must be one of 'historical', 'future', or 'present'.")
  }
  
  #load data 
  df = readClimateData(lat,lng, data.dir, ...)
  
  #vector of dates and years
  df$date = dateVector(years[1], years[2])
  df$year = as.integer(format(df$date, "%Y"))
    
  #calculate GDD
  #df$gdd = calcGDD(df$temp.high, df$temp.low)
  
  #return the data frame
  df
}

#' Get crops available for a given location.
#' 
#' Given a lat/lng pair, lookup which crop ids are produced
#' in that area. Return the cropids.
#' 
#' @param lat the latitude.
#' @param lng the longitude.
#' @param data.dir the directory of the crop lookup data.
#' @return a list of cropids.
#' @importFrom dplyr filter
#' @export
getLocationCrops <- function(lat, lng, data.dir="data/") {
  load(paste0(data.dir,"locationCrops.rda"))
  filter(locationCrops, latitude==lat & longitude==lng)$cropid
}




#' Fetch raw data
#' 
#' Get raw data from WSU servers and store it in data-raw/
#' 
#' @export
#' @param username login username
#' @param password login password
#' @param server server name, default aeolus.wsu.edu 
#' @param save.dir directory to save data to
#' @param file.paths list of file paths
#' @param file.names list of file names to download
#' @return TRUE if successful
#' @importFrom stringi stri_locate_last_words
fetchRawData <- function(
    username=NULL, 
    password=NULL, 
    server="aeolus.wsu.edu", 
    save.dir = "data/",
    file.names = c("/data/kirti/vic_inputdata0625_pnw_combined_05142008.tar.gz",
      "/data/jennylabcommon/Future2035MetData/ccsm3_B1_2020-2049.tar.gz",
      #"/data/jennylabcommon/Future2035MetData/cgcm3.1_t47_B1_2020-2049.tar.gz",
      #"/data/jennylabcommon/Future2035MetData/hadcm_B1_2020-2049.tar.gz",
      #"/data/jennylabcommon/Future2035MetData/ipsl_cm4_A1B_2020-2049.tar.gz",
      #"/data/jennylabcommon/Future2035MetData/pcm1_A1B_2020-2049.tar.gz",
      #"HistoricalCropGrids.txt",
      "CropParamsForNick/crops.tar.gz"
      )
    ) {
  
  #check that we have username and password
  if (!interactive() && (is.null(username) || is.null(password))) {
    stop("Not in interactive mode and username and/or password not set.")
  }
  
  #get it if not
  if (is.null(username)) username = readline("Username: ")
  if (is.null(password)) password = readline("Password: ")
 
  #fetch the data
  for (infile in file.names) {
    #names
    name.index = stri_locate_last_words(infile)
    file.name = substr(infile,name.index[1], name.index[2])
    outfile = file(paste0(save.dir,file.name), 'wb')
    
    #connection
    x = scp(server, fn, user=paste0(username,":",password), binary=TRUE)
    
    #write to file
    writeBin(x, outfile)
  }

  TRUE
}









