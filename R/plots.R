# Plots
#require(ggplot2)
#require(gridExtra)

#' Create a plot for a specific location from scratch.
#' 
#' Loads the necessary data and creates a plot from it.
#' 
#' @export
#' @param lat the latitude of a location.
#' @param lng the longitude of a location.
#' @param crop the name of a crop.
#' @param years.range a 2-item list of the first and last 
#'   year to include in the range.
#' @param year.this the year to consider the current year.
#' @param years.compare list of years to compare to.
#' @param today the date to consider today.
#' @param forecast.date the data at which forecast starts.
#' @return plot object
createPlotFromLocation <- function(lat, 
                       lng,
                       crop,
                       years.this, 
                       years.range=c(1970,2006),
                       years.compare=NULL, 
                       today=Sys.Date(), 
                       forecast.date=NULL) {
  #load location data and calculate some measures
  byyear = getLocationData(lat, lng, years.range[1], years.range[2])
  byyear$gdd = with(byyear, calcGDD(temp.high, temp.low, crop$temp.base))
  byyear$forecast = with(byyear, as.integer((date>=today)))
  byyear$monthday = as.factor(format(byyear$date, "%b-%d"))
  
  #get daily ranges for variables
  byday = createDailyData(byyear)
  
  
  
  # create GDD plot
  pgdd = makeGDDPlot(byday, dplyr::filter(byyear, as.integer(format(date,"%Y"))==years.this), crop$silk, crop$flower)
    
  # add compare years
  for (y in years.compare) {
    pgdd + addYearToPlot(dplyr::filter(df.years, as.integer(format(date, "%Y"))==y))
  }
  pgdd
}

#' make the central plot
#' 
#' Plots the climate data as well as yield estimates and growth stage points.
#' 
#' @param df a data frame containing gdd variables.
#' @param gdd.silk (numeric) the gdd level needed to silk (specific to a crop)
#' @param gdd.flower (numeric) the gdd level needed to flower (specific to a crop)
#' 
#' @return a plot object made with ggplot2
#' @export
#' @import ggplot2
#' @importFrom scales date_format
#' 
createPlotFromClimateData <- function(df.historical,
                        df.present,
                        df.future = NULL,
                        crop,
                        today=format(Sys.Date(), "%Y-%m-%d")) {
  
  #Calculated gdd for each date based on crop information
  df.historical$gdd = calcGDD(df.historical$temp.hi, 
                              df.historical$temp.low)
  #, 
  #                            crop$base_temp, 
  #                            25, 
  #                            crop$max_temp, temp.min)
  df.present$gdd = calcGDD(df.present$temp.hi, 
                           df.present$temp.low)
                           #, 
                           #crop$base_temp, 
                           #25, 
                           #crop$max_temp, temp.min)
  
  #assign a date to historical maxes and mins based on monthday
  df.historical = createDailyData(df.historical)
  year = df.present$year[1]
  df.historical$date = as.Date(paste0(year,"-",df.historical$monthday))
 
  #present data includes a forecast if after forecast date, which is today... 
  df.present$forecast = with(df.present, as.integer((date>=today)))
  df.present$monthday = as.factor(format(df.present$date, "%b-%d"))
  
  #find date of significant milestones
  #date.senescence = df.present[cumsum(df.present$gdd)>crop$senescence, 'date'][1]
  #date.flowering = df.present[cumsum(df.present$gdd)>crop$flowering, 'date'][1]
  
  #Plot
  p <- ggplot(data=df.historical, aes(x=date)) + 
    # GDD range
    geom_ribbon(aes(ymin=cumsum(gdd.min), ymax=cumsum(gdd.max)), color='#99ccff', alpha=0.1) +
    geom_line(aes(y=cumsum(gdd.avg)), color="gray", size=1.1) +
    
    # This year's gdd
    geom_line(data=df.present, aes(y=cumsum(gdd), x=date, linetype=factor(forecast)), size=1) + 
    
    # Silking
#     geom_segment(x=as.integer(df.present$date[1]), 
#                  xend=as.integer(date.senescence), 
#                  y=crop$senescence, 
#                  yend=crop$senescence, 
#                  color='red', 
#                  size=0.5) +
#     geom_segment(x=as.integer(date.senescence), 
#                  xend=as.integer(date.senescence), 
#                  y=0, 
#                  yend=crop$senescence, 
#                  color='red', 
#                  size=0.5) +
#     # Flowering
#     geom_segment(x=as.integer(df.present$date[1]), 
#                  xend=as.integer(date.flowering), 
#                  y=crop$flowering, 
#                  yend=crop$flowering, 
#                  color='blue', 
#                  size=0.5,
#                  alpha=0.1) +
#     geom_segment(x=as.integer(date.flowering), 
#                  xend=as.integer(date.flowering), 
#                  y=0, 
#                  yend=crop$flowering, 
#                  color='blue', 
#                  size=0.5,
#                  alpha=0.9) +
#     
    # harvest day
    # first freeze day (histogram)
    # last freeze day (histogram)
    
    # x axis scale
    scale_x_date(breaks="1 month", minor_breaks=NULL, labels=(scales::date_format("%b"))) +
    #scale_x_discrete(breaks=c("Jan-01", "Feb-01", "Mar-01", "Apr-01", "May-01", "Jun-01",
    #                          "Jul-01", "Aug-01", "Sep-01", "Oct-01", "Nov-01", "Dec-01")) +
    
    # labels
    xlab("Month") + ylab("Growing Degree Days")
  
  setTheme(p) #+ theme()
}

#' Plot temperature.
#' 
#' Plot high/low, average, and this year's data
#'
#' @export 
#' @param df a data frame containing variables: temp.max, temp.min, temp.high, temp.low
#' @return a ggplot2 object
makeTempPlot <- function(df,
                         tmax.varname = temp.max) {
  # Temerature high and low
  p <- ggplot(data=df, aes(x=date)) +
    # max and min
    geom_line(aes(y=temp.max), type='.', alpha=0.1) + 
    geom_line(aes(y=temp.min), type='.', alpha=0.1) +
    geom_ribbon(aes(ymin=temp.min, ymax=temp.max), color='#aaaaaa', alpha=0.5) +
    #scale_color_gradient2(low='blue', high='red', midpoint=65) +
     
    # this year
    geom_line(aes(y=temp.high), color='red') + 
    geom_line(aes(y=temp.low), color='blue') +
    geom_ribbon(aes(ymin=temp.low, ymax=temp.high), color='red', alpha=0.5) +
    
    # horizontal lines for the GDD limits
    geom_hline(aes(yintercept=86), color='#999999') +
    geom_hline(aes(yintercept=50), color='#999999') +
    
    # scale
    scale_x_date(minor_breaks=NULL) +
    scale_y_continuous(minor_breaks=NULL) +
    
    # labels
    xlab("") + ylab("Temperature")
 
  setTheme(p)
}

#' A blank placeholder plot.
#' 
#' A blank plot to fill space for gridded multi-plots.
#' 
#' @export
#' @return a ggplot2 object
makeBlankPlot <- function() {
  #blank plot to take up space
  ggplot() + geom_blank()
}

#' Make a multi-plot plot.
#' 
#' Make a multi-plot object including temp and gdd.
#' 
#' @export
#' @param df a data frame containing temperature and gdd variables
#' @return a ggplot2 object
makePlot <- function(df ) {
  
  gddPlot = makeGDDPlot(df)
  tempPlot = makeTempPlot(df)
  blankPlot = makeBlankPlot()
  
  grid.arrange(gddPlot, tempPlot, ncol=1, nrow=2, widths=c(4), heights=c(4,1.4))
}

#' Theme for cbccy plots.
#' 
#' Sets the default theme for cbccy plots.
#'
#' @export
#' @param p a ggplot2 object 
#' @return a modified ggplot2 object 
setTheme <- function(p) {
  p + theme(axis.title.x=element_blank(),           #remove xaxis title
            panel.background=element_blank(),       #background white
            axis.line=element_line(color="black", size=0.5),   #lines black
            legend.position="none"                  #no legend
  )
}

