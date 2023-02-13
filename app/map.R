library(shiny)
library(leaflet)


# Create a dataset with the coordinates of some cities
cities_df <- data.frame(
  city = c("New York", "London", "Paris", "Berlin"),
  lat = c(40.7128, 51.5074, 48.8566, 52.5200),
  lon = c(-74.0060, -0.1278, 2.3522, 13.4050)
)

# Define the UI
ui <- fluidPage(
  titlePanel("Interactive Map"),
  
  # Add a leaflet map to the UI
  leafletOutput("map", width = "100%", height = "600px"),
  
  # Add a select input for the cities
  selectInput("city", "Select a city:", choices = cities_df$city)
)

# Define the server
server <- function(input, output) {
  
  # Create the leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      setView( lng = -73.935242, lat = 40.730610, zoom = 12 ) %>% 
      addProviderTiles("Esri.WorldGrayCanvas") %>% 
      addMarkers(
        lng = cities_df$lon,
        lat = cities_df$lat,
        popup = cities_df$city,
        label = cities_df$city
      )
  })
  
  # Add the selected city marker to the map
  observeEvent(input$city, {
    city_coords <- cities_df[cities_df$city == input$city, c("lat", "lon")]
    leafletProxy("map", data = city_coords) %>%
      clearMarkers() %>%
      addMarkers(
        lng = city_coords$lon,
        lat = city_coords$lat,
        popup = input$city,
        label = input$city,
        clusterOptions = markerClusterOptions()
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)
