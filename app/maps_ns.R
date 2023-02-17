
###############################Install Related Packages #######################
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}
if (!require("leafsync")) {
  install.packages("leafsync")
  library(leafsync)
}
if (!require("shinydashboard")) {
  install.packages("shinydashboard")
  library(shinydashboard)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("reshape2")) {
  install.packages("reshape2")
  library(reshape2)
}
if (!require("tidytext")) {
  install.packages("tidytext")
  library(tidytext)
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("tm")) {
  install.packages("tm")
  library(tm)
}

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
source('global.R')
df_unique

#Data Prepocessing
year_options = c(2016, 2017,
                 2018, 2019,
                 2020, 2021,
                 2022)

df <- filter(df_unique,inspection_year %in% year_options)

# Define UI

ui <- fluidPage(
  titlePanel("Interactive Map"),
  
  # Add a leaflet map to the UI
  leafletOutput("map", width = "100%", height = "600px"),
  
  # Add a select input for the cities
  fluidRow(column(width = 3,
         box(width = NULL,  
             selectInput(
               inputId = "years",
               label = "Inspection year:",
               choices = year_options
             )
             
         )
)
)
)

# Define Server
server <- shinyServer(function(input, output) {
  
  #maps
  output$map <- renderLeaflet({
    data <- df
    color <- colorFactor(topo.colors(2), data$violation_type)
    leaflet(data) %>%
      setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%

      addLegend(
        "bottomleft", # Legend position
        pal=color, # color palette
        values=~data$violation_type, # legend values
        opacity = 1,
        title="Violation Type"
      )
    
  })
  
  observeEvent(input$years, {
    years_picked <- df[df$inspection_year == input$years, ]
    leafletProxy("map", data = years_picked) %>%
    addCircleMarkers(
      lng=~years_picked$longitude, # Longitude coordinates
      lat=~years_picked$latitude, # Latitude coordinates
      radius=4,
      stroke=FALSE, # Circle stroke
      fillOpacity=0.5, # Circle Fill Opacity
      # color=~color(years_picked$violation_type),
      popup=~paste(
        "<b>", years_picked$restaurant_name , "</b><br/>",
        "Violation Type : ", as.character(years_picked$violation_type), "<br/>",
        "Address: ", as.character(years_picked$building), " ", as.character(years_picked$street), "<br/>",
        "Inspection Date: ", as.character(years_picked$inspection_date), "<br/>",
        "Violation Description: ", as.character(years_picked$violation_description)
      )
    )
      })
  })

# Run the Shiny app
shinyApp(ui, server)