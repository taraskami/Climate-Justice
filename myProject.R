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

world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005))

mytext <- paste(
  "Country: ", world_spdf@data$NAME,"<br/>", 
  "CO2 Emissions Per Capita: ", world_spdf@data$POP2005, 
  sep="") %>%
  lapply(htmltools::HTML)

mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$POP2005, na.color="transparent", bins=mybins)

m <- leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(POP2005), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~POP2005, opacity=0.9, title = "Carbon Emissions Per Capita (CO2)", position = "bottomleft" )



# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello World!"),
  
  checkboxGroupInput("variable", "Variables to show:",
                     c("CO2 Emissions Per Capita" = "CO2",
                       "Temperature Rise" = "Temp")),
  
  leafletOutput("midtermmap"),
  p(),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  output$midtermmap <- renderLeaflet({
    
    if (length(input$variable) == 0) {
      leaflet(world_spdf) %>% 
        addTiles() %>% 
        setView( lat=10, lng=0 , zoom=2)
    } else if (input$variable == "CO2") {
      m
    }
    
  })
  
  
}

shinyApp(ui = ui, server = server)