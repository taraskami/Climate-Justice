library(shiny)
library(leaflet)
library(RColorBrewer)
library(shiny)
library(leaflet)
library(rgdal)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

mybins <- c(0,2.5,5,7.5,10,12.5,15,17.5,20,30,40,50)
world_spdf <- readOGR(dsn="/Users/nicklamanna/Documents/App-1/Data/world_shape_file", 
                      layer="TM_WORLD_BORDERS_SIMPL-0.3",
                      verbose=FALSE
)

mytext <- paste(
  "Country: ", world_spdf@data$NAME,"<br/>", 
  "Area: ", only2017[], "<br/>", 
  "Population: ", round(world_spdf@data$POP2005, 2), 
  sep="") %>%
  lapply(htmltools::HTML)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("x", "Year:",min = 1980, max = 2017, value=1980,step=1, animate=animationOptions(3300)),textOutput("SliderText"),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$x[1] & quakes$mag <= input$x[2],]
    world_spdf@data[1:246,input$x[1]-1968]
  })
  
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  mytext <- reactive({paste(
    "Country: ", world_spdf@data$NAME,"<br/>", 
    "CO2 Emissions Per Capita: ", filteredData(), 
    sep="") %>%
    lapply(htmltools::HTML)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(world_spdf) %>% 
      addTiles()  %>% 
      setView( lat=10, lng=0 , zoom=2)
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    #pal <- colorpal()
    
    mypalette <- colorBin( palette="YlOrBr", domain=filteredData(), na.color="transparent", bins=mybins)
    leafletProxy("map", data = world_spdf) %>%
      clearShapes() %>%
      addPolygons( 
        fillColor = ~mypalette(filteredData()),
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="white", 
        weight=0.3,
        label = mytext(),
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        ))
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = quakes)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      mypalette <- colorBin( palette="YlOrBr", domain=filteredData(), na.color="transparent", bins=mybins)
      proxy %>% addLegend(pal=mypalette, values=~filteredData(), opacity=0.9, title = "Carbon Emissions Per Capita (CO2)", position = "bottomleft")
    }
  })
  
}

shinyApp(ui, server)