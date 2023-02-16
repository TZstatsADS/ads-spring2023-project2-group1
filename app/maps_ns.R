
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

#Data Prepocessing
library(readr)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
year_options = c("2016", "2017",
                 "2018", "2019",
                 "2020", "2021",
                 "2022")
restaurant_inspections <- read_csv("../data/DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
restaurant_inspections_filtered <- restaurant_inspections[!(is.na(restaurant_inspections$`VIOLATION DESCRIPTION`) | restaurant_inspections$`VIOLATION DESCRIPTION`==""), ]

restaurant_inspections_filtered_1 <- restaurant_inspections_filtered[!(is.na(restaurant_inspections_filtered$Latitude) | restaurant_inspections_filtered$Latitude=="" | 
                                                                         is.na(restaurant_inspections_filtered$Longitude) | restaurant_inspections_filtered$Longitude=="") , ]
restaurant_inspections_filtered_1$dateandtime <- as.POSIXct(restaurant_inspections_filtered_1$`INSPECTION DATE`, format = "%m/%d/%Y")
restaurant_inspections_filtered_1$years <- format(restaurant_inspections_filtered_1$dateandtime, format = "%Y")
restaurant_inspections_filtered_2 <- restaurant_inspections_filtered_1[which(restaurant_inspections_filtered_1$years %in% year_options),]


restaurant_inspections_filtered_3 <- restaurant_inspections_filtered_2[order(restaurant_inspections_filtered_2$DBA, 
                                                                             as.Date(restaurant_inspections_filtered_2$`INSPECTION DATE`, format = "%m/%d/%Y")),]

restaurant_inspections_filtered_3$criticalviolations <- ifelse(restaurant_inspections_filtered_3$`CRITICAL FLAG` == "Critical", 1, 0)
restaurant_inspections_filtered_3$noncriticalviolations <- ifelse(restaurant_inspections_filtered_3$`CRITICAL FLAG` == "Not Critical", 1, 0)


restaurant_inspections_filtered_3$ACTION <- ifelse(restaurant_inspections_filtered_3$ACTION == "Violations were cited in the following area(s).", 
                                                   "Violations", ifelse(restaurant_inspections_filtered_3$ACTION == "No violations were recorded at the time of this inspection.",
                                                                        "No violations", ifelse(restaurant_inspections_filtered_3$ACTION == "Establishment Closed by DOHMH. Violations were cited in the following area(s) and those requiring immediate action were addressed.",
                                                                                                "Closed", ifelse(restaurant_inspections_filtered_3$ACTION == "Establishment re-closed by DOHMH.", "Re-closed",
                                                                                                                 "Re-opened")
                                                                        )
                                                   ))
restaurant_inspections_filtered_year <- filter(restaurant_inspections_filtered_3,years %in% year_options)
restaurant_inspections_filtered_year$months <- format(restaurant_inspections_filtered_year$dateandtime, format = "%m-%Y")
df1 <- aggregate(cbind(restaurant_inspections_filtered_year$criticalviolations,restaurant_inspections_filtered_year$noncriticalviolations) ~ 
                   restaurant_inspections_filtered_year$DBA, data = restaurant_inspections_filtered_year, FUN = sum, na.rm = TRUE)

df1$criticalYN <- ifelse(df1$V1 > df1$V2, "Critical", "Non-Critical")
df1$DBA <- df1$`restaurant_inspections_filtered_year$DBA`
df1$critical <- df1$V1
df1$noncritical <- df1$V2
df1 <-subset(df1, select = -c(V1, V2, `restaurant_inspections_filtered_year$DBA`))

total <- merge(df1, restaurant_inspections_filtered_year,by="DBA")
total1 <- total %>% distinct(DBA, .keep_all = TRUE)

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
               label = "Choose a Year:",
               choices = total1$years,
               selected = "2019"
             )
             
         )
)
)
)

# Define Server
server <- shinyServer(function(input, output) {
  
  #maps
  output$map <- renderLeaflet({
    data <- total1
    color <- colorFactor(topo.colors(2), data$criticalYN)
    leaflet(data) %>%
      setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        lng=~data$Longitude, # Longitude coordinates
        lat=~data$Latitude, # Latitude coordinates
        radius=~ifelse(data$criticalYN == "Critical", 2, 2), # Total count
        stroke=FALSE, # Circle stroke
        fillOpacity=1, # Circle Fill Opacity
        color=~color(data$criticalYN),
        popup=~paste(
          "<b>", data$DBA , "</b><br/>",
          "criticalYN : ", as.character(data$criticalYN), "<br/>",
          "Borough:", as.character(data$BORO),
          "date: ", as.character(data$dateandtime)
        )
        # Popup content
      ) %>%
      addLegend(
        "bottomleft", # Legend position
        pal=color, # color palette
        values=~data$criticalYN, # legend values
        opacity = 1,
        title="Critical Violation"
      )
    
  })
  
 # observeEvent(input$years, {
 #   years_picked <- total1[total1$years == input$years]
 #   leafletProxy("map", data = years_picked) %>%
 #     clearMarkers() %>%

        # Popup content
 #     )
 # })
  
})


# Run the Shiny app
shinyApp(ui, server)

