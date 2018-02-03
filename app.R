# from: https://rstudio.github.io/leaflet/shiny.html
library(shiny)
library(leaflet)
library(RColorBrewer)
library(sf)

cycle_infra = read_sf("data/tfgm-cycle-infra.geojson")
l = read_sf("data/routes_car.geojson")
cent = st_centroid(l)
l$Minutes = l$car_time - l$bicycle_time + 8
schools = load("data/Schools.Rdata")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Minutes saved", -1, 15,
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
      addPolylines(data = l[l$Minutes > input$range[1] & l$Minutes < input$range[2], ], group = "l", color = ) %>% 
      addPolylines(data = cycle_infra, group = "infra", color = "grey") %>% 
      addLayersControl(
        overlayGroups = c("infra", "l"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      hideGroup("infra")
      
    if(input$purpose != "Commuting") {
      proxy %>% 
        clearShapes() %>% 
        addCircleMarkers(lng = School.data$lon, lat = School.data$lat, group = "school")
       }
  })

}

shinyApp(ui, server)