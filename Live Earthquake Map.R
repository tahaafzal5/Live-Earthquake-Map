library(RCurl)
library(RJSONIO)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(jsonlite)

# get all the URLs
urlPastHour = getURL("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson")
urlPastDay = getURL("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_day.geojson")
urlPastWeek = getURL("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_week.geojson")
urlPast30Days = getURL("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_month.geojson")

# put them in a vector to make it suitable for later use
inputID = c(urlPastHour, urlPastDay, urlPastWeek, urlPast30Days)

# create a default data frame to display as user opens app first.
# read the JSON files and start navigating to the coords and magnitudes.
link = urlPast30Days
data = jsonlite::fromJSON(link, flatten = TRUE)
size = dim(data$features[1])

# create an empty dataframe for later.
dataFrame = data.frame()

for (i in rep(1:size))
{
  # get the latitudes and longitudes, put them in a dataframe.
  tempCoords = data$features$geometry.coordinates[[i]]
  record = data.frame(tempCoords[2], tempCoords[1], tempCoords[3], data$features$properties.mag[i])
  
  # merge the created data frame with the empty data frame
  dataFrame = rbind(dataFrame, record)
}

# rename the column names of the dataFrame.
colnames(dataFrame) = c("lat", "long", "depth", "mag")

# create a function to process the non-default data sets.
# getQuakes takes a string to choose and loads the data corresponding to that string.
getQuakes<-function(str){
  if(str == "Past 30 Days"){
    link = urlPast30Days
  }
  else if(str == "Past Week"){
    link = urlPastWeek
  }
  else if(str == "Past Day"){
    link = urlPastDay
  }
  else if(str == "Past Hour"){
    link = urlPastHour
  }
  
  # do the same data collection and manipulation above to get the complete data frame to work on.
  data = jsonlite::fromJSON(link, flatten = TRUE)
  size = dim(data$features[1])
  
  dataFrame = data.frame()
  for (i in rep(1:size))
  {
    tempCoords = data$features$geometry.coordinates[[i]]
    record = data.frame(tempCoords[2], tempCoords[1], tempCoords[3], data$features$properties.mag[i])
    dataFrame = rbind(dataFrame, record)
  }
  colnames(dataFrame) = c("lat", "long", "depth", "mag")
  return(dataFrame)
}

# set-up the UI of the page.
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min(dataFrame$mag), max(dataFrame$mag),
                            value = range(dataFrame$mag), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show Legend", TRUE),
                selectInput("inputID", "TimeFrame", list("Past Hour", "Past Day", "Past Week", "Past 30 Days"), selected = "Past 30 Days", multiple = FALSE)
  )
)

# create the server side
server <- function(input, output, session) 
{
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    getQuakes(input$inputID)[getQuakes(input$inputID)$mag >= input$range[1] & getQuakes(input$inputID)$mag <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, getQuakes(input$inputID)$mag)
  })
  # updateSelectInput(session, "inputID", label = "TimeFrame", choices = list("Past Hour", "Past Day", "Past Week", "Past 30 Days"),  selected = "Past 30 Days")
  
  output$inp<-({
    renderText(input$inputID)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(data = getQuakes(input$inputID)) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = getQuakes(input$inputID))
    
    # Remove any existing legend, and only if the legend is  
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
  })
}

shinyApp(ui, server)