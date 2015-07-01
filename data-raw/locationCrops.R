#Link between crops and locations, process and save to an R object.

library(dplyr)

#working directory
setwd("/home/potterzot/reason/work/wsu/climate-change/cbccy/")

file.name = "data-raw/HistoricalCropGrids.txt"

df = NULL
for (line.str in readLines(file(file.name, 'r'))) {
  linesplit = strsplit(line.str, " ")[[1]]
  line = list()
  line$unknown = linesplit[1]
  line$num.crops = linesplit[2]
  loc = strsplit(linesplit[3], "_")[[1]]
  line$latitude = loc[2]
  line$longitude = loc[3]
   
  #for each crop... 
  for (cropid in 4:length(linesplit)) {
    loc.crop = line
    loc.crop$cropid = linesplit[cropid]
    if (is.null(df)) df = as.data.frame(loc.crop, stringsAsFactors=FALSE)
    else df = bind_rows(df, as.data.frame(loc.crop, stringsAsFactors=FALSE))
  }
}

save(df, file="data/locationCrops.rda")