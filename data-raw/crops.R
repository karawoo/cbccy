#Build example dataset for packaging

library(dplyr)


#working directory
setwd("/home/potterzot/reason/work/wsu/climate-change/cbccy/")

#list of crop files
files = list.files("data-raw/crops/")

df = NULL
for (crop.file in files) {
  crop = list()
  crop$rawname = strsplit(crop.file,".")[[1]]
  
  f = file(paste0("data-raw/crops/",crop.file), 'r')
  for (line in readLines(f)) {
    if (substr(line,1,1)!="[") { #don't get the 'section' separators
      keyvalpair = strsplit(line,"=")
      crop[keyvalpair[[1]][1]] = keyvalpair[[1]][2]
    }
  }
  close(f)
  
  # copy to data frame of all crops
  if (is.null(df)) {
    df = as.data.frame(crop, stringsAsFactors=FALSE)
  }
  else {
    df = bind_rows(df,as.data.frame(crop, stringsAsFactors=FALSE))
  }
}
save(df, file="data/crops.rda")
