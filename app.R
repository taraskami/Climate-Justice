library(shiny)
library(leaflet)
library(shinyWidgets)
library(jsonlite)
library(rgdal)


# countries <- readOGR(dsn="./data/world_shape_file", 
#                      layer="TM_WORLD_BORDERS_SIMPL-0.3",
#                      verbose=FALSE)

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
            actionButton("action", "Responsibility Index"),
            actionButton("action", "CO2 Emissions"),
            actionButton("action", "Temperature Rise"),
            actionButton("action", "Natural Disaster Fatalities"),
            actionButton("action", "Population"),   
        )
    ),
    
    leafletOutput("map", width = "100%", height = "100%")
    
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
    
)

server <- function(input, output, session) {
    
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