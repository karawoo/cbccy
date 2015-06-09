#global.R holds the main data processing and setup for the application

#library(dplyr)

#List of latitude and longitude pairs. Add to this list to add a new data point. The corresponding data file should also be added to the data folder and be appropriately named.
latLngList = list(
  c(48.40625, -122.40625),
  c(48.53125, -122.46875),
  c(47.59375, -120.21875),
  c(48.34375, -122.34375),
  c(48.40625, -122.34375)
  )

id = seq(1,length(latLngList))

latLngData = data.frame(id, mapply(c,latLngList)[1,],mapply(c,latLngList)[2,]) 
names(latLngData) <- c('id', 'lat', 'lng')