library(RCurl)
library(RJSONIO)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(jsonlite)

urlPastHour = getURL("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson")
urlPastDay = getURL("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_day.geojson")
urlPastWeek = getURL("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_week.geojson")
urlPast30Days = getURL("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_month.geojson")

inputID = c(urlPastHour, urlPastDay, urlPastWeek, urlPast30Days)

solve<-function(str){
  if(str == "Past 30 Days"){
    data = jsonlite::fromJSON(urlPast30Days, flatten = TRUE)
  }
  else if(str == "Past Week"){
    data = jsonlite::fromJSON(urlPastWeek, flatten = TRUE)
  }
  else if(str == "Past Day"){
    data = jsonlite::fromJSON(urlPastDay, flatten = TRUE)
  }
  else if(str == "Past Hour"){
    data = jsonlite::fromJSON(urlPastHour, flatten = TRUE)
  }
  
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

server <- function(input, output, session) 
{
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    solve(input$inputID)[solve(input$inputID)$mag >= input$range[1] & solve(input$inputID)$mag <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, solve(input$inputID)$mag)
  })
  
  output$inp<-({
    renderText(input$inputID)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(data = solve(input$inputID)) %>% addTiles() %>%
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
    proxy <- leafletProxy("map", data = solve(input$inputID))
    
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