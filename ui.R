#Libraries for use
library(shiny)
library(leaflet)
library(ggplot2)

# Choices for drop-downs
years = seq(2036,2054)
crops <- sort(c("Barley"="Barley",
  "Corn"="Corn",
  "Oats"="Oats", 
  "Spring Wheat"="SpringWheat",
  "Winter Wheat"="WinterWheat", 
  "Alfalfa"="Alfalfa",
  "Pasture"="Pasture", 
  "Pasutre (Hay)"="PastureHay", 
  "Apples"="Apples",
  "Cherries"="Cherry",
  "Grapes"="Grapes",
  "Potatoes"="Potatoes", 
  "Grape Juice"="GrapeJuice"))

shinyUI(navbarPage("GDD Explorer", id="nav",
  tabPanel("Interactive map",
    div(class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("style.css"),
        includeScript("gomap.js")
      ),
      
      #map underlayer
      leafletOutput("map", width="100%", height="100%"),
      
      #graph overlayer
      absolutePanel(id="controls", class="panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = "30%", height = "auto",
        
        h2("Annual Growing Degree Days"),
        
        #selectInput("year", "Year", years, selected=2036),
        selectInput("crop", "Crop", crops, selected="Corn"),
        
        plotOutput("cumGDD", height=400)
      ),
      
      #Citation for data and everything else
      tags$div(id="cite",
        'Data from ', tags$em('INSERT TITLE HERE'), ' by INSERT AUTHOR.'
      )
    )
  ),

  tabPanel("Data explorer",
    fluidRow(
      column(3,
        selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        )
      )
    ),
    fluidRow(
      column(1,
        numericInput("minScore", "Min score", min=0, max=100, value=0)
      ),
      column(1,
        numericInput("maxScore", "Max score", min=0, max=100, value=100)
      )
    ),
    hr(),
    DT::dataTableOutput("ziptable")
  ),

  conditionalPanel("false", icon("crosshair"))
))

