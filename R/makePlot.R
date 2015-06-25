# Plots
require(ggplot2)
require(gridExtra)

#' theme for the plot
#' 
#' Sets the design of the plot so that it only has to be called once.
setTheme <- function(p) {
  p + theme(axis.title.x=element_blank(),           #remove xaxis title
            panel.background=element_blank(),       #background white
            axis.line=element_line(color="black", size=0.5),   #lines black
            legend.position="none"                  #no legend
            )
}


#' make the central plot
#' 
#' Plots the climate data as well as yield estimates and growth stage points.
#' 
#' @param df a data frame containing the necessary variables
#' @return a plot object made with ggplot2
makeGDDPlot <- function(df) {
  silk.gdd = 1000
  silk.date = as.Date(df[cumsum(df$gdd)>silk.gdd, 'date'][1])
  flower.gdd = 3000
  flower.date = as.Date(df[cumsum(df$gdd)>flower.gdd, 'date'][1])
  p <- ggplot(data=df, aes(x=date)) + 
    # GDD
    geom_ribbon(aes(ymin=cumsum(gdd.min), ymax=cumsum(gdd.max)), color='#99ccff', alpha=0.1) +
    geom_line(aes(y=cumsum(gdd), linetype=factor(forecast)), size=2) + 
    geom_line(aes(y=cumsum(gdd.avg)), color="gray", size=1.1) +
   
    # Silking
    geom_segment(x=as.integer(df$date[1]), 
                 xend=as.integer(silk.date), 
                 y=silk.gdd, 
                 yend=silk.gdd, 
                 color='red', 
                 size=0.5) +
    geom_segment(x=as.integer(silk.date), 
                 xend=as.integer(silk.date), 
                 y=0, 
                 yend=silk.gdd, 
                 color='red', 
                 size=0.5) +
    # Flowering
    geom_segment(x=as.integer(df$date[1]), 
                 xend=as.integer(flower.date), 
                 y=flower.gdd, 
                 yend=flower.gdd, 
                 color='blue', 
                 size=0.5,
                 alpha=0.1) +
    geom_segment(x=as.integer(flower.date), 
                 xend=as.integer(flower.date), 
                 y=0, 
                 yend=flower.gdd, 
                 color='blue', 
                 size=0.5,
                 alpha=0.9) +
    
    # x axis scale
    scale_x_date(minor_breaks=NULL) +
    
    # labels
    xlab("Month") + ylab("Growing Degree Days")
  
  # silking day (earliest, current, latest)
  #geom_(aes(yintercept=crop.silk))
  #geom_bar(aes(y=silk, group=year), stat="identity") +
  #geom_bar(aes(y=harvest, group=year), stat="identity")
  
  # harvest day
  # first freeze day (histogram)
  # last freeze day (histogram)
  
  setTheme(p) #+ theme()
}

makeTempPlot <- function(df) {
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

makeBlankPlot <- function() {
  #blank plot to take up space
  ggplot() + geom_blank()
}

makePlot <- function(df) {
  
  gddPlot = makeGDDPlot(df)
  tempPlot = makeTempPlot(df)
  blankPlot = makeBlankPlot()
  
  grid.arrange(gddPlot, tempPlot, ncol=1, nrow=2, widths=c(4), heights=c(4,1.4))
  

}