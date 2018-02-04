# from: https://rstudio.github.io/leaflet/shiny.html
library(shiny)
library(leaflet)
library(RColorBrewer)
library(sf)

cycle_infra = read_sf("data/tfgm-cycle-infra.geojson")
routes_car = read_sf("data/routes_car.geojson")
lines = read_sf("data/lines.geojson")
lines_car = subset(lines, fastest == "car")
lines_bicycle = subset(lines, fastest == "bike")
lines_transit = subset(lines, fastest == "transit")
cent = st_centroid(routes_car)
routes_car$Minutes = routes_car$car_time - routes_car$bicycle_time + 8
schools = load("data/Schools.Rdata")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Minutes saved", -1, 15,
                            value = c(0, 15), step = 0.1
                ),
                selectInput("purpose", "Journey Purpose",
                            c("Commuting", "Travel to School")
                )
                # checkboxInput("legend", "Show legend", TRUE)
  )
)

top500line <- function(proxy, data, name, colour) {
  # Complains about invalid map parameter
  # weightedLine <- function(data, proxy, name, color) {
  #   addPolylines(map = proxy, data = data$geometry, group = paste("Top 500 by", name), color = colour, weight = data$all / 20000, opacity = 1)
  # }

  # apply(data, 1, weightedLine, proxy, name, color)

  addPolylines(proxy, data = data, group = paste("Top 500 by", name), color = colour, weight = 2, opacity = 1)
}

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(lng = -2.24, 53.48, zoom = 13)
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    proxy = leafletProxy("map") %>% 
      clearShapes() %>% 
      clearMarkers() %>% 
      hideGroup(c("Cycle infrastructure", "Top 500 by bike", "Top 500 by public transit", "Top 500 by car"))
      
    if (input$purpose == "Commuting") {
      proxy %>%
        addPolylines(data = routes_car[routes_car$Minutes > input$range[1] & routes_car$Minutes < input$range[2], ], group = "Car routes", color = ) %>%
        addPolylines(data = cycle_infra, group = "Cycle infrastructure", color = "grey") %>%
        top500line(lines_bicycle, "bike", "green") %>%
        top500line(lines_transit, "public transit", "yellow") %>%
        top500line(lines_car, "car", "red") %>%
        addLayersControl(
          overlayGroups = c("Cycle infrastructure", "Car routes", "Top 500 by bike", "Top 500 by public transit", "Top 500 by car"),
          options = layersControlOptions(collapsed = FALSE),
          position = "topleft"
        )
    }
    else if (input$purpose == "Travel to School") {
      proxy %>% 
        addCircleMarkers(lng = School.data$lon, lat = School.data$lat, group = "Schools")
       }
  })

}

shinyApp(ui, server)
