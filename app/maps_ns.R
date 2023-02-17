
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


# Load data
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
source('global.R')

#Data Prepocessing
year_options = c(2016, 2017,
                 2018, 2019,
                 2020, 2021,
                 2022)

df <- filter(df_unique,inspection_year %in% year_options)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Map with Filters"),
  sidebarLayout(
    sidebarPanel(
      selectInput("borough", "Select Borough:", 
                  choices = unique(df$boro), 
                  selected = "Manhattan"),
      selectInput(
        inputId = "years",
        label = "Inspection Year:",
        choices = year_options),
      selectInput(
        inputId = "viol",
        label = "Violation Type:",
        choices = unique(df$violation_type))
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    df %>%
      filter(boro == input$borough,
             inspection_year == input$years,
             violation_type == input$viol) %>%
      mutate(violation_type = as.factor(violation_type)) 
  })
  
  colors_map <- c("No Violation" = "green", "Not-Critical" = "yellow", "Critical" = "orange", "Not Applicable" = "grey")
  colors_2 <- c("orange","grey", "green", "yellow")
  
  # Render map
  output$map <- renderLeaflet({
    leaflet(filtered_data()) %>%
      setView(lng = -73.98928, lat = 40.75042, zoom = 12) %>%
      addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png") %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        ~filtered_data()$longitude
        , ~filtered_data()$latitude
        , radius=4
        , stroke=FALSE # Circle stroke
        , fillOpacity=0.5 # Circle Fill Opacity
        , fillColor = colors_2[as.integer(filtered_data()$violation_type)]
        , popup = ~paste(
          "<b>", filtered_data()$restaurant_name , "</b><br/>",
          "Violation Type : ", as.character(filtered_data()$violation_type), "<br/>",
          "Address: ", as.character(filtered_data()$building), " ", as.character(filtered_data()$street), "<br/>",
          "Inspection Date: ", as.character(filtered_data()$inspection_date), "<br/>",
          "Violation Description: ", as.character(filtered_data()$violation_description))
      )%>%
      addLegend(
        "bottomleft", # Legend position
        colors = colors_map, # color palette
        labels = names(colors_map), # legend labels
        opacity = 1,
        title = "Violation Type"
      )
  })
  
}

# Run app
shinyApp(ui, server)
