# data.R functions for working with climate data

#' makePlot() creates a plot for the app
#'
#' generate an interactive plot using ggplot2.
#' 
#' @param df a data frame
#' 
createChartData <- function(df, this.year) {
  #Forecast is from today forward
  today = as.Date("2012-06-20")
  df$forecast = 0
  df[df$date>=today, 'forecast'] = 1
  
  df$gdd = calcGDD(df$temp.high, df$temp.low)
  #df$gdd.forecast = df$gdd #past is unknown
  #df[df$date>=today, 'gdd'] = NA #future is unknown
  #df[df$date<today, 'gdd.forecast'] = NA #past is unknown
 
  # day dimension
  df.by_day = group_by(df, monthday)
  by.day = summarize(df.by_day, 
                     temp.max=max(temp.high),
                     temp.avg=mean(temp.avg),
                     temp.min=min(temp.low),
                     precip.max = max(precip),
                     precip.min = min(precip),
                     windspeed.max = max(windspeed),
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
  arrange(merge(by.day, filter(df, year==this.year), by="monthday"), date)
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
getData <- function(year.start, year.end) {
  
}

#' Load climate data.
#' 
#' Loads climate data for a specific latitude/longitude pair.
#' 
#' @param lat a latitude
#' @param lng a longitude
#' @param data.dir a directory location, default "data/locations/"
#' @param file.name.root filename roots, default "data_"
#' @param column.names data frame column names, default 
#'   c("precipitation", "temp.max", "temp.min", "windspeed")
#' @param column.factors a vector of values to multiply by
#' @return a data frame of climate data
getClimateData <- function(lat, 
                             lng, 
                             data.dir="data/locations/", 
                             file.name.root = "data_",
                             column.names = c("precipitation", 
                                              "temp.max", 
                                              "temp.min", 
                                              "windspeed"),
                             column.factors = c(0.4, .01, .01, .01)
                             ) {
  #load the data
  file.name = paste0(data.dir,file.name.root,lat,"_",lng)
  climateVector <- readBinaryData(file.name)
  
  #parse to data frame
  row.num = length(climateVector)/column.num
  column.num = length(column.names)
  df = data.frame(matrix(climateVector, nrow=row.num, ncol=column.num))
  names(df) <- column.names
  
  #factors
  for (i in 1:column.num) {
    df[,column.names[i]] = df[,column.names[i]] * column.factors[i]
  }
  df
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
readBinaryData <- function(file.name, 
                           what="integer", 
                           size=2, 
                           n=134412, 
                           encoding="ASCII") {
  
  to.read = file(file.name, 'rb', encoding=encoding)
  x = readBin(to.read,what=what, size=size, n=n, ...)
}

#' Get data from historical file
#' 
#' Gets data from history, including specific years of history
#' 
#' @param lat the latitude of a location
#' @param lng the longitude of a location
#' @param year.start the first year of data to include, default 1915
#' @param year.end the last year of data to include, default 2006
#' @return a data frame of climate data from 1915-2006
getHistoryData <- function(lat, lng, year.start=1915, year.end=2006, ...) {
  df = getClimateData(lat,lng, ...)
  
  #since data files are not dated, get the whole file, then filter to years we want.
  df$date = dateVector(1915, 2006)
  filter(df, 
         date <= as.Date(paste0(year.end, "-12-31")) && 
           date >= as.Date(paste0(year.start, "-01-01")))
}

#' Fetch raw data
#' 
#' Get raw data from WSU servers and store it in data/
#' @param username login username
#' @param password login password
#' @param server server name, default aeolus.wsu.edu 
#' @param save.dir directory to save data to
#' @param file.paths list of file paths
#' @param file.names list of file names to download
#' @return TRUE if successful
fetchRawData <- function(
    username=NULL, 
    password=NULL, 
    server="aeolus.wsu.edu", 
    save.dir = "data/",
    file.paths = c(
      "/data/kirti/",
      "/data/jennylabcommon/Future2035MetData/",
      "/data/jennylabcommon/Future2035MetData/",
      "/data/jennylabcommon/Future2035MetData/",
      "/data/jennylabcommon/Future2035MetData/",
      "/data/jennylabcommon/Future2035MetData/"
      ),
    file.names = c("vic_inputdata0625_pnw_combined_05142008.tar.gz",
      "ccsm3_B1_2020-2049.tar.gz",
      "cgcm3.1_t47_B1_2020-2049.tar.gz",
      "hadcm_B1_2020-2049.tar.gz",
      "ipsl_cm4_A1B_2020-2049.tar.gz",
      "pcm1_A1B_2020-2049.tar.gz"
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
  for (i in 1:length(file.names)) {
    #names
    inname = paste0(file.paths[i],file.names[i])
    outname = paste0(save.dir,file.names[i])
    outfile = file(outname, 'wb')
    
    #connection
    x = scp(server, fn, user=paste0(username,":",password), binary=TRUE)
    
    #write to file
    writeBin(x, outfile)
    
  }

  TRUE
}









