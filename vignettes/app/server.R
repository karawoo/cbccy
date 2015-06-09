library(shiny)
library(leaflet)
library(ggplot2)
library(lattice)
#library(RColorBrewer)
#library(scales)
#library(dplyr)

## Variables ##

#File name stuf
dataDir = "data/"
fileNameRoot = "MIROC_rcp85_"
fileNameSuffix = ".txt"

#Data column names
columnNames <- c("year", "month", "day", "average_temperature", "Barley", "Corn", "Oats", "SpringWheat", "WinterWheat", "Alfalfa", "Pasture", "PastureHay", "Apples", "Cherry", "Grapes", "Potatoes", "GrapeJuice")

#Load the latitude and longitude pairs
locations <- latLngData
startLng = locations[1,3]
startLat = locations[1,2]

# Create the server logic
shinyServer(function(input, output, session) {
  #Function to fetch data
  getData <- function(lat,lng) {
    fileName = paste0(dataDir,fileNameRoot,lat,"_",lng,fileNameSuffix)
    gdd <- read.table(fileName, sep='\t')
    names(gdd) <- columnNames
    gdd$date = as.Date(strptime(paste0(gdd$year,"-",gdd$month,"-",gdd$day), format="%Y-%m-%d"))
    gdd$monthDay = as.Date(strptime(paste0(gdd$month,"-",gdd$day), format="%m-%d"))
    return(gdd)
  }
  
  # Load initial data
  gddData = getData(startLat, startLng)
  
  
  ## Interactive Map ## 
  circleColor = '#981e32'
  r = 5000 #radius
  map = leaflet () %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%
    addMarkers(data=locations) %>%
    #addCirlces(data=locations, radius=r, stroke=FALSE, fillOpacity=0.6, fillColor=circleColor) %>%
    setView(lng = startLng, lat = startLat, zoom = 7)
  
  output$map <- renderLeaflet(map)

  # Plot for GDD
  #data = getData(lng, lat)
  output$cumGDD <- renderPlot({  
    selectedData = selectData()
    #print(summary(selectedData))
    if (nrow(selectedData)==0)
      return(NULL)
  
    p <- ggplot(data=selectedData, aes(x=monthDay, y=gdd, color=year)) +
      geom_line() +
      geom_point() +
      ylab('Growing Degree Days') +
      xlab('Month')
    print(p)

  })
  
  #Reactive function to select limit the data
  selectData <- reactive({
    if (is.null(input$crop))
      return(gddData[FALSE,])
    
    sData = gddData[c('year','month','day','monthDay','average_temperature')]
    sData['gdd'] = gddData[input$crop]
    sData
  })
  

  
  # Pop Up data for selected location
  showLatLngPopup <- function(lat, lng) {
    content <- as.character(tagList(
      tags$h4("Climate data for ", 2036),
      tags$h4("Longitude:", lng)
      #tags$h4("Score:", as.integer(selectedZip$centile)),
      #tags$strong(HTML(sprintf("%s, %s %s",
      #  selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
      #))), 
      #tags$br(),
      #sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), 
      #tags$br(),
      #sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), 
      #tags$br(),
      #sprintf("Adult population: %s", selectedZip$adultpop)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = 'gdd')
  }
  
  # When a location is clicked, load data on that location.
  #observeEvent(input$map_marker_click, function() {
  #  leafletProxy("map") %>% setView(lng=event$lng, lat=event$lat, zoom=9)
  #})
  
  #Observer to popup on hover
  observe({
    event <- input$map_marker_mouseover
    if (is.null(event))
      return()
    
    isolate({
      showLatLngPopup(event$lat, event$lng)
    })
  })
  
  #Observer to close popup on mouseout
  observe({
    event <- input$map_marker_mouseout
    if (is.null(event))
      return()

    leafletProxy("map") %>% clearPopups()
  })
  
  #Observer to change data on click
  observe({
    #leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    #print(input)
    if (is.null(event))
      return()
    
    leafletProxy("map") %>% setView(lng=event$lng, lat=event$lat, zoom=9)
    
    gddData <- getData(event$lat, event$lng)
    selectData()
  })
})  