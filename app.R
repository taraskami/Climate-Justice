library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
    titlePanel("Climate Justice"),
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session) {
    
    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        leaflet() %>%
            setView(lng = 0, lat = 0, zoom = 3) %>%
            addTiles()
    })
}

shinyApp(ui, server)