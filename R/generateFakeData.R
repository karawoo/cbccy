genFreeze <- function() {
  
}

genTrend <- function(num.x, shift=1, scale=1, mean=60, sd=35, factor=4000) {
  years = num.x / 364.25
  x1 = -years*scale*pi+shift
  x2 = years*scale*pi+shift
  step = (x2-x1)/num.x
  x = mean + sd*sin(seq(x1, x2, step)) #temperature as sine wave!
  x = jitter(x, factor=factor)
  x[1:num.x]
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
  day0 = as.Date("2010-01-01")
  num.years = 4
  date = as.Date(day0 + 0:(365*(num.years))) #-1 - floor(num.years/4)
  num.dates = length(date)
  
  df = data.frame(date)
  df['year'] = as.integer(format(date, "%Y"))
  df['monthday'] = format(date, "%b-%d")
  #df['day'] = as.integer(format(dates, "%d"))
  #df['mdl'] = format(dates, "%b-%d")
  #df['mdn'] = as.numeric(format(dates, "%m%d"))
  #df['dayn']  = 

  #temperatures
  df['temp.high'] = genTrend(num.dates, shift=4, mean=60)
  df['temp.low'] = genTrend(num.dates, shift=4, mean=35)
  df['temp.avg'] = (df$temp.high + df$temp.low)/2
  
  #growing degree days
  #gdd.base = 50
  #df['gdd'] = calcGDD(df[,'temp.high'], df[,'temp.low'], gdd.base)
  #df['cgdd'] = calcCumGDD(dates, df['gdd'])
  
  #precipitation 
  df['precip'] = genTrend(num.dates, scale=2, mean=50, sd=10)
  
  #wind speed
  df['windspeed'] = genTrend(num.dates, shift=pi, mean=10, sd=10)
  
   
  #first and last freeze
  #df$freeze.last = genFreeze()
  #df$freeze.first = genFreeze()
  df
  
}




