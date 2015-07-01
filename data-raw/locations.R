#Build example dataset for packaging

#working directory
setwd("/home/potterzot/reason/work/wsu/climate-change/cbccy/")

#archive directories
archive.historical.dir = ""
archive.future.dir = ""

#list of locations to copy
locations = list(
  c(46.28125, -116.21875),
  c(46.71875, -117.21875),
  c(47.21875, -117.21875),
  c(48.40625, -117.84375)
)

#make a list of location files to extract
future.files = c()
historical.files = c()
for (i in 1:length(locations)) {
  loc = locations[[i]]
  future.files[i] = paste0(archive.future.dir, "data_",loc[1],"_",loc[2]) 
  historical.files[i] = paste0(archive.historical.dir, "data_",loc[1],"_",loc[2]) 
}

#Extract historical data
untar("data-raw/vic_inputdata0625_pnw_combined_05142008.tar.gz", 
      historical.files, 
      exdir = "data/historical/")

#Extract future data
untar("data-raw/ccsm3_B1_2020-2049.tar.gz", 
      future.files, 
      exdir = "data/future/")
