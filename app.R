library(shiny)
library(leaflet)
library(shinyWidgets)
library(jsonlite)
library(rgdal)


countries <- readOGR(dsn="./data/world_shape_file",
                     layer="TM_WORLD_BORDERS_SIMPL-0.3",
                     verbose=FALSE)
map_data <- read.csv("map_data.csv")
only2017 <- read.csv("only2017.csv")
countries@data$TempPrediction <- only2017$TempPrediction
countries@data$TempActuals <- only2017$TempActuals
countries@data$Difference <- only2017$Difference

world_spdf <- readOGR(dsn="./Data/world_shape_file", 
                      layer="TM_WORLD_BORDERS_SIMPL-0.3",
                      verbose=FALSE
)

for (i in 12:87) {
    world_spdf@data[1:246,i][ which(world_spdf@data[1:246,i] == 0)] = NA
}


pal <- colorQuantile(palette="YlOrRd", domain=only2017$Difference)

mytext <- paste(
    "Country: ", countries@data$NAME, "</br>",
    "CO2 (2017): ", countries@data$CO2017, "</br>",
    "Predicted Temperature:", 
        ifelse(countries@data$TempPrediction == 0,
               "Missing temp data", countries@data$TempPrediction),
    "</br>",
    "Actual Temperature:",
    ifelse(countries@data$TempActuals == 0,
           "Missing temp data", countries@data$TempActuals),
   "</br>",
    "Temperature Difference: ", countries@data$Difference
) %>%
    lapply(htmltools::HTML)



ui <- bootstrapPage(
    setBackgroundColor("#15940C"),
    tags$style("
        
        @import url('https://fonts.googleapis.com/css?family=Sora');
        
        html, body {
            width:100%;
            height:100%;
        }
        
        #header {
            font-family:Sora;
        }
        
        #action {
            background-color:#15940C;
        }
        
        #filter {
            padding: 0px 20px 10px 50px;
        }
        
        .btn {
            border-radius: 25px;
            border: 2px solid #73AD21;
        }
        
        .btn:hover {
            background-color: #457A60;
        }
    "),
    headerPanel(
        tags$h1("Climate Justice", id = "header", align="center")
    ),
    
    fluidRow(
        tags$div(
            id = "filter",
            actionButton("action1", "Home"),
            actionButton("action2", "CO2 Emissions"),
            actionButton("action3", "Net Temperature Change"),
            actionButton("action", "Natural Disaster Fatalities"),
            actionButton("action", "Population"),   
        )
    ),
    
    leafletOutput("map", width = "100%", height = "100%"),
    
    # absolutePanel(
    #     fixed = TRUE,
    #     draggable = TRUE,
    #     top = 60,
    #     left = "auto",
    #     right = 20,
    #     bottom = "auto",
    #     width = 330,
    #     height = "auto",
    #     h2("ZIP explorer"),
    # )
    
    absolutePanel(top = 130, right = 70,
                  sliderInput("x", "Year:",min = 1980, max = 2017, value=1980,step=1),textOutput("SliderText"),
                  checkboxInput("legend", "Show legend", TRUE),
    )
    
    
)

server <- function(input, output, session) {
    
    observeEvent(input$action1, {
        
        output$map <- renderLeaflet({
            # Use leaflet() here, and only include aspects of the map that
            # won't need to change dynamically (at least, not unless the
            # entire map is being torn down and recreated).
            leaflet(countries) %>%
                setView(lng = 0, lat = 0, zoom = 3) %>%
                addTiles() %>%
                addPolygons(fillOpacity = 1, fillColor = ~pal(countries@data$Difference),
                            label = mytext,
                            labelOptions = labelOptions( 
                                style = list("font-weight" = "normal", padding = "3px 8px"), 
                                textsize = "13px", 
                                direction = "auto"
                            ))
            
        })
        
        
    })
        
    observeEvent(input$action2, {
        mybins <- c(0,5,7.5,10,12.5,15,20,40,70)
        
        # Reactive expression for the data subsetted to what the user selected
        filteredData <- reactive({
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
                proxy %>% addLegend(pal=mypalette, values=~filteredData(), opacity=0.9, title = "Carbon Emissions Metric Tons Per Capita (CO2)", position = "bottomleft")
            }
        })
    })
    
    observeEvent(input$action3, {
        mybins <- c(0,0.25,0.5,0.75,1,1.25,1.5,2.0,3.0,5.0)
        
        filteredData <- reactive({
            world_spdf@data[1:246,input$x[1]-1930]
        })
        
        # This reactive expression represents the palette function,
        # which changes as the user makes selections in UI.
        colorpal <- reactive({
            colorNumeric(input$colors, quakes$mag)
        })
        
        mytext <- reactive({paste(
            "Country: ", world_spdf@data$NAME,"<br/>", 
            "Net Temperature Change in Degrees Celcius: ", filteredData(), 
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
                proxy %>% addLegend(pal=mypalette, values=~filteredData(), opacity=0.9, title = "Net Temperature Change (Degrees Celcius)", position = "bottomleft")
            }
        })
    })
    
    
    
    
    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        leaflet(countries) %>%
            setView(lng = 0, lat = 0, zoom = 3) %>%
            addTiles() %>%
            addPolygons(fillOpacity = 1, fillColor = ~pal(countries@data$Difference),
                        label = mytext,
                        labelOptions = labelOptions( 
                            style = list("font-weight" = "normal", padding = "3px 8px"), 
                            textsize = "13px", 
                            direction = "auto"
                            ))
        
    })
}

shinyApp(ui, server)