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
if (!require("wordcloud")) {
  install.packages("wordcloud")
  library(wordcloud)
}
if (!require("stringr")) {
  install.packages("stringr")
  library(stringr)
}
if (!require("repr")) {
  install.packages("repr")
  library(repr)
}
if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}





###############################Load The Data #######################

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
source('global.R')

#Data Prepocessing
year_options = c(2016, 2017,
                 2018, 2019,
                 2020, 2021,
                 2022)

df <- filter(df_unique,inspection_year %in% year_options)
colors_map <- c("Critical" = "orange", "Not-Critical" = "yellow", "Not Applicable" = "grey", "No Violation" = "green")

'%like%' <- function(x, pattern) {
  grepl(pattern, x, ignore.case = TRUE)
}

###############################Define UI #######################

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Roboto', sans-serif;
        font-size: 14px;
      }
      .navbar-default {
        background-color: #f5f5f5;
      }
      .nav-link {
        color: #333333;
        font-weight: bold;
      }
      .nav-link:hover, .nav-link:focus {
        color: #7fad39;
      }
      .tab-content {
        background-color: #f9f9f9;
        border: 1px solid #dddddd;
        padding: 20px;
        border-radius: 5px;
      }
      h1 {
        margin-top: 0;
        margin-bottom: 20px;
        font-size: 24px;
        font-weight: bold;
      }
      p {
        margin-bottom: 20px;
      }
      .form-control {
        font-size: 14px;
        height: 36px;
        padding: 6px 12px;
        border-radius: 5px;
        border: 1px solid #cccccc;
        box-shadow: none;
      }
      .form-group {
        margin-bottom: 20px;
      }
      .leaflet-container {
        height: 600px;
      }
      .leaflet-popup-content {
        font-size: 14px;
      }
      .leaflet-tooltip {
        font-size: 14px;
      }
    "))
  ),
  titlePanel("Everything Good and Healthy in NYC"),
  tabsetPanel(
    
    # Tab 1
    tabPanel("Introduction", value = "Introduction",
             h1("Introduction"),
             p("This is the introduction tab. Here you can provide some background or context for your app.")
    ),
    
    # Tab 2
    tabPanel("Government Initiatives", value = "Government Initiatives",
             h1("Government Initiatives"),
             p("This is the government initiatives tab. Here you can discuss any related government programs, policies, or initiatives."),
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "borough",
                             label = "Choose a borough:",
                             choices = c("Manhattan", "Brooklyn",
                                         "Staten Island", "Queens",
                                         "Bronx"))
               ),
               mainPanel(
                 plotOutput(outputId = "plot1"),
                 plotOutput(outputId = "plot2")
               )
             )),
    
    # Tab 3
    tabPanel("The Results", value = "The Results",
             h1("The Results"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("borough", "Select Borough:", 
                             choices = unique(df$boro),
                             selected = 'Manhattan',
                             multiple = TRUE),
                 selectInput(
                   inputId = "years",
                   label = "Inspection Year:",
                   choices = year_options,
                   selected = 2022,
                   multiple = TRUE),
                 selectInput(
                   inputId = "grade",
                   label = "Inspection Grade:",
                   choices = unique(df$grade),
                   selected = 'C',
                   multiple = TRUE),
                 selectInput(
                   inputId = "viol",
                   label = "Violation Type:",
                   choices = unique(df$violation_type),
                   selected = 'Critical',
                   multiple = TRUE),
                 textInput("rest_name", 
                           label = "Restaurant Name:", 
                           value = "",
                           placeholder = "Enter restaurant name"),
                 actionButton("search", "Search", class = "btn btn-success btn-block")
               ),
               mainPanel(
                 leafletOutput("map"),
                 div(id = "result-table")
               )
             )
    )
  )
)


###############################Define Server #######################

server <- function(input, output) {
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    # Filter by selected borough, inspection year, and violation type
    data <- df %>%
      filter(boro %in% input$borough,
             inspection_year %in% input$years,
             violation_type %in% input$viol,
             grade %in% input$grade)
    
    # Filter by restaurant name if text input is not empty
    if (input$rest_name != "") {
      data <- data %>% filter(str_to_lower(restaurant_name) %like% str_to_lower(input$rest_name))
    }
    
    # Convert violation type to factor with ordered levels for color mapping
    data$violation_type <- factor(data$violation_type, levels = names(colors_map))
    data
  })
  
  colors_map <- c("No Violation" = "forestgreen", "Not-Critical" = "gold", "Critical" = "orange", "Not Applicable" = "grey")
  colors_2 <- c("forestgreen","gold", "orange", "grey")
  
  # Render Map
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
        , fillOpacity=0.3 # Circle Fill Opacity
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
  
  #Render Barplot
  
  da <- read.csv("/Users/namirasuniaprita/Documents/GitHub/ads-spring2023-project2-group1/data/Data Inspection Result_cleaned.csv")
  borough_data <- reactive({
    da<-da %>% filter(da$BORO %in% input$borough)
  })
  
  output$plot1=renderPlot({
    data2 = borough_data()
    ggplot(data2, aes(x = factor(GRADE)))+  
      geom_bar(width = 0.9) + coord_flip() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 200))
  })
  
  #Render Worldcloud
  
  output$plot2=renderPlot({
    dw = borough_data()
    
    par(mfrow = c(3, 3))
    
    #Filtering
    
    df1<-dw |>
      filter(VIOLATION.CODE=="02B")
    wc_data1 = 
      df1 |>
      unnest_tokens(output = word, input = VIOLATION.DESCRIPTION)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data1$word, freq = wc_data1$n,scale = c(2,0.5),max.words = 200,rot.per = 0)
    
    df2<-dw |>
      filter(VIOLATION.CODE=="02G")
    wc_data2 = 
      df2 |>
      unnest_tokens(output = word, input = VIOLATION.DESCRIPTION)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data2$word, freq = wc_data2$n,scale = c(2,0.5),max.words = 200,rot.per = 0)
    
    df3<-dw |>
      filter(VIOLATION.CODE=="04L")
    wc_data3 = 
      df3 |>
      unnest_tokens(output = word, input = VIOLATION.DESCRIPTION)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data3$word, freq = wc_data3$n,scale = c(2,0.5),max.words = 200,rot.per = 0)
    
    df4<-dw |>
      filter(VIOLATION.CODE=="04N")
    wc_data4 = 
      df4 |>
      unnest_tokens(output = word, input = VIOLATION.DESCRIPTION)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data4$word, freq = wc_data4$n,scale = c(2,0.5),max.words = 200,rot.per = 0)
    
    df5<-dw |>
      filter(VIOLATION.CODE=="06C")
    wc_data5 = 
      df5 |>
      unnest_tokens(output = word, input = VIOLATION.DESCRIPTION)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data5$word, freq = wc_data5$n,scale = c(2,0.5),max.words = 200,rot.per = 0)
    
    df6<-dw |>
      filter(VIOLATION.CODE=="06D")
    wc_data6 = 
      df6 |>
      unnest_tokens(output = word, input = VIOLATION.DESCRIPTION)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data6$word, freq = wc_data6$n,scale = c(2,0.5),max.words = 200,rot.per = 0)
    
    df7<-dw |>
      filter(VIOLATION.CODE=="08A")
    wc_data7 = 
      df7 |>
      unnest_tokens(output = word, input = VIOLATION.DESCRIPTION)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data7$word, freq = wc_data7$n,scale = c(2,0.5),max.words = 200,rot.per = 0)
    
    df8<-dw |>
      filter(VIOLATION.CODE=="10B")
    wc_data8 = 
      df8 |>
      unnest_tokens(output = word, input = VIOLATION.DESCRIPTION)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data8$word, freq = wc_data8$n,scale = c(2,0.5),max.words = 200,rot.per = 0)
    
    df9<-dw |>
      filter(VIOLATION.CODE=="10F")
    wc_data9 = 
      df9 |>
      unnest_tokens(output = word, input = VIOLATION.DESCRIPTION)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data9$word, freq = wc_data9$n,scale = c(2,0.5),max.words = 200,rot.per = 0)    
  })
  
}

###############################Run App#######################
shinyApp(ui, server)
