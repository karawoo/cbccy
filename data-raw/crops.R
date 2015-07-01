#Build crops for packaging

#working directory
setwd("/home/potterzot/reason/work/wsu/climate-change/cbccy/")

#crop ids
cropids = read.csv("data-raw/cropids.txt", sep="\t", col.names=c("id", "rawname"))

#crop names
cropnames = read.csv("data-raw/cropnames.txt", sep="\t", col.names=(c("name", "rawname")))

#list of crop files
files = untar("data-raw/crops.tar.gz", list=TRUE)

df = NULL
for (crop.file in files) {
  crop = list()
  crop.name.length = stringi::stri_locate_first_words(crop.file, ".CS_crop")[2]
  crop$rawname = substr(crop.file,3,crop.name.length)
  
  #extract the file
  untar("data-raw/crops.tar.gz", files=crop.file, exdir="data-raw/") 
  
  #get the data from the file
  f = file(paste0("data-raw/",crop.file), 'r')
  for (line in readLines(f)) {
    if (substr(line,1,1)!="[") { #don't get the 'section' separators
      keyvalpair = strsplit(line,"=")
      crop[keyvalpair[[1]][1]] = keyvalpair[[1]][2]
    }
  }
  close(f)
  
  # remove the file
  file.remove(paste0("data-raw/", crop.file))
  
  # copy to data frame of all crops
  if (is.null(df)) {
    df = as.data.frame(crop, stringsAsFactors=FALSE)
  }
  else {
    df = bind_rows(df,as.data.frame(crop, stringsAsFactors=FALSE))
  }
}
df = merge(df, cropids)
crops = merge(df, cropnames)
save(crops, file="data/crops.rda")
