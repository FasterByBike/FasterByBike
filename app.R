# from: https://rstudio.github.io/leaflet/shiny.html
library(shiny)
library(leaflet)
library(RColorBrewer)
library(sf)

cycle_infra = read_sf("data/tfgm-cycle-infra.geojson")
l = read_sf("data/routes_car.geojson")
l$Minutes = l$car_time - l$bicycle_time
schools = load("data/Schools.Rdata")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Minutes saved", -10, 10,
                            value = c(0, 5), step = 0.1
                ),
                selectInput("purpose", "Journey Purpose",
                            c("Commuting", "Travel to School")
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% addTiles() %>%
      addPolylines(data = l[l$Minutes > input$range[1] & l$Minutes < input$range[2], ], group = "l")
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    proxy = leafletProxy("map")
    if(input$purpose != "Commuting") {
      proxy %>% 
        addCircleMarkers(lng = School.data$lon, lat = School.data$lat) %>% 
        addPolylines(data = cycle_infra, group = "infra") %>%
        addLayersControl(
          overlayGroups = c("infra", "l"),
          options = layersControlOptions(collapsed = FALSE)
        )
       }
  })

}

shinyApp(ui, server)