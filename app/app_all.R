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
if (!require("lubridate")) {
  install.packages("lubridate")
  library(lubridate)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
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
df_no_mod_2022 <- filter(df_no_mod, inspection_year==2022)
df_unique_2022 <- filter(df_unique, inspection_year==2022)

'%like%' <- function(x, pattern) {
  grepl(pattern, x, ignore.case = TRUE)
}



###############################Define UI #######################

ui <- fluidPage(
  theme = shinytheme("flatly"), # Apply the Flatly theme
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Roboto', sans-serif;
        font-size: 14px;
      }
      .navbar {
        background-color: #fff;
        border: none;
      }
      .navbar-brand, .navbar-nav li a {
        color: #333;
        font-weight: bold;
      }
      .nav-link:hover, .nav-link:focus {
        color: #7fad39;
      }
      .tab-content {
        background-color: #fff;
        border: none;
        padding: 20px;
        border-radius: 5px;
        box-shadow: 0px 0px 8px rgba(0, 0, 0, 0.1);
      }
      h1 {
        margin-top: 0;
        margin-bottom: 20px;
        font-size: 24px;
        font-weight: bold;
        color: #333;
      }
      p {
        margin-bottom: 20px;
        color: #333;
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
    id = "tabs",
    type = "pills",
    
    # Tab 1
    tabPanel("Introduction", value = "Introduction",
             h1("About The App"),
             p("Eating healthy isn't about eating more or eating less, it's about eating right! Our restaurant guide will give you a rundown of all the hygiene practices and things 'to know and beware of', before visiting restaurants in the city. The information we present is brought to you by inspections conducted by the Department of Health and Mental Hygiene. We care for where you eat!"),
             p("We'll give you the scoop on the number of restaurants with grade A, B, C, and beyond (because let's face it, sometimes a C is just a fancy way of saying 'gross')."),
             mainPanel(plotOutput(outputId = "plot1"),
                       plotOutput(outputId = "plot2")),
             p("D: This designation is given to restaurants that have a number of violations that pose a public health hazard."),
             p("N: This designation is given to restaurants that have violations that are not considered to be a public health hazard."),
             p("P: This designation is given to restaurants that are in the process of re-opening after being closed for health code violations."),
             p("Z: This designation is given to restaurants that have not yet been inspected.")
             
    )
    ,
    
    # Tab 2
    tabPanel("Government Initiatives", value = "Government Initiatives",
             h1("Inspections on Restaurants by Borough and Cuisine Type"),
             p("With recent news about the government ramping up restaurant inspections in NYC and the city council passing a bill to ensure food delivery apps display accurate health inspection grades, our app provides the number of inspections from 2019 to 2022. Plus, the Health Department's new letter grading system, which includes an 'A+' grade for restaurants with no violations in the past year, makes it even easier to find the cleanest eateries around."),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "borough", label = "Select Borough:",
                             choices = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")),
                 selectInput(inputId = "cursine_type", label = "Select Cuisine Type:",
                             choices = c("Chinese", "American", "Italian", "Japanese","Korean")),
               ),
               mainPanel(
                 plotOutput(outputId = "Plot3"),
                 plotOutput(outputId = "Plot4")
               )
             )
    ),
    
    
    # Tab 3
    tabPanel("The Results", value = "The Results",
             h1("Exploring the City's Cuisine with Confidence"),
             p("Hungry in the city that never sleeps? Now you can make informed decisions about where to eat, hygiene wise. Don't be fooled by those fancy menus and dim lighting – we'll tell you which places have the squeakiest clean kitchens and which to avoid like the plague (or, you know, food poisoning). Whether you're a seasoned foodie or just looking for a quick bite, this page is the perfect companion for any hungry adventurer. Let's explore the city one meal at a time!"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("boro", "Select Borough:", 
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
                           placeholder = "Enter restaurant name..."),
                 textInput("viol_desc", 
                           label = "Violation Description:", 
                           value = "",
                           placeholder = "Try rats or flies..."),
                 actionButton("search", "Search", class = "btn btn-success btn-block")
               ),
               mainPanel(
                 leafletOutput("map", width = "100%", height = "590px"),
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
      filter(boro %in% input$boro,
             inspection_year %in% input$years,
             violation_type %in% input$viol,
             grade %in% input$grade)
    
    # Filter by restaurant name if text input is not empty
    if (input$rest_name != "") {
      data <- data %>% filter(str_to_lower(restaurant_name) %like% str_to_lower(input$rest_name))
    }
    if (input$viol_desc != "") {
      data <- data %>% filter(str_to_lower(violation_description) %like% str_to_lower(input$viol_desc))
    }
    
    # Convert violation type to factor with ordered levels for color mapping
    data$violation_type <- factor(data$violation_type, levels = names(colors_map))
    data
  })
  
  colors_map <- c("No Violation" = "forestgreen", "Not-Critical" = "gold", "Critical" = "orange", "Not Applicable" = "grey")
  colors_2 <- c("forestgreen","gold", "orange", "grey")
  
  colors_map <- c("No Violation" = "forestgreen", "Not-Critical" = "gold", "Critical" = "orange", "Not Applicable" = "grey")
  colors_2 <- c("forestgreen","gold", "orange", "grey")
  
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
  output$plot1=renderPlot({
    data2 = df_unique_2022 %>% filter(df_unique_2022$grade=='A' | df_unique_2022$grade=='B' |df_unique_2022$grade=='C' | df_unique_2022$grade=='N' | df_unique_2022$grade=='P' | df_unique_2022$grade=='Z')
    ggplot(data2, aes(x = factor(grade), fill=factor(ifelse(grade=="A","Restaurants with Grade A","Restaurants without Grade A"))))+  
      geom_bar(width = 0.9) + coord_flip() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 200))+
      labs(x="Numbers of Restaurant",y="Grade")+
      scale_fill_manual(name = "grade", values=c("red","grey50"))
  })
  
  #Render Worldcloud
  output$plot2=renderPlot({
    dw = df_no_mod_2022
    
    par(mfrow = c(3, 3))
    
    #Filtering
    
    df1<-dw |>
      filter(violation_code=="02B")
    wc_data1 = 
      df1 |>
      unnest_tokens(output = word, input = violation_description)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data1$word, freq = wc_data1$n,scale = c(2,0.5),max.words = 200,rot.per = 0, colors="Red")
    
    df2<-dw |>
      filter(violation_code=="02G")
    wc_data2 = 
      df2 |>
      unnest_tokens(output = word, input = violation_description)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data2$word, freq = wc_data2$n,scale = c(2,0.5),max.words = 200,rot.per = 0, colors="Red")
    
    df3<-dw |>
      filter(violation_code=="04L")
    wc_data3 = 
      df3 |>
      unnest_tokens(output = word, input = violation_description)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data3$word, freq = wc_data3$n,scale = c(2,0.5),max.words = 200,rot.per = 0, colors="Red")
    
    df4<-dw |>
      filter(violation_code=="04N")
    wc_data4 = 
      df4 |>
      unnest_tokens(output = word, input = violation_description)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data4$word, freq = wc_data4$n,scale = c(2,0.5),max.words = 200,rot.per = 0, colors="Red")
    
    df5<-dw |>
      filter(violation_code=="06C")
    wc_data5 = 
      df5 |>
      unnest_tokens(output = word, input = violation_description)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data5$word, freq = wc_data5$n,scale = c(2,0.5),max.words = 200,rot.per = 0, colors="Red")
    
    df6<-dw |>
      filter(violation_code=="06D")
    wc_data6 = 
      df6 |>
      unnest_tokens(output = word, input = violation_description)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data6$word, freq = wc_data6$n,scale = c(2,0.5),max.words = 200,rot.per = 0, colors="Red")
    
    df7<-dw |>
      filter(violation_code=="08A")
    wc_data7 = 
      df7 |>
      unnest_tokens(output = word, input = violation_description)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data7$word, freq = wc_data7$n,scale = c(2,0.5),max.words = 200,rot.per = 0, colors="Red")
    
    df8<-dw |>
      filter(violation_code=="10B")
    wc_data8 = 
      df8 |>
      unnest_tokens(output = word, input = violation_description)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data8$word, freq = wc_data8$n,scale = c(2,0.5),max.words = 200,rot.per = 0, colors="Red")
    
    df9<-dw |>
      filter(violation_code=="10F")
    wc_data9 = 
      df9 |>
      unnest_tokens(output = word, input = violation_description)|>
      anti_join(y = stop_words)|>
      group_by(word)|>
      summarize(n = n())|>
      ungroup()|>
      arrange(desc(n))
    wordcloud(words = wc_data9$word, freq = wc_data9$n,scale = c(2,0.5),max.words = 200,rot.per = 0, colors="Red")    
  })
  
  # Render Bar Chart
  
  ds = df_barchart
  ds= ds %>% filter(grepl("2020", ds$INSPECTION.DATE) | grepl("2019", ds$INSPECTION.DATE) | grepl("2021", ds$INSPECTION.DATE))
  ds = ds %>% filter(ds$CUISINE.DESCRIPTION =="Chinese"|ds$CUISINE.DESCRIPTION =="Korean"|ds$CUISINE.DESCRIPTION =="Japanese"|ds$CUISINE.DESCRIPTION =="American"|ds$CUISINE.DESCRIPTION =="Italian") %>% group_by(BORO) %>% select(BORO,INSPECTION.DATE,CUISINE.DESCRIPTION)
  data1 <- count(ds %>% group_by(INSPECTION.DATE) %>% filter(CUISINE.DESCRIPTION == "Korean"))
  data1 = data1 %>% arrange(mdy(data1$INSPECTION.DATE))
  data1$INSPECTION.DATE <- as.Date(data1$INSPECTION.DATE, format="%m/%d/%Y")
  print(ggplot(data1,aes(x = data1$INSPECTION.DATE, y = data1$n, group = 1))+geom_line(color = "steelblue")+labs(x="Date",y="Number of Inspections") + labs(title = paste("The relationship between Time and number of Inspections on Restaurants in ")) + scale_x_date())
  
  borough_data2 <- reactive({
    if ( "Manhattan" %in% input$borough){
      data = count(ds %>% group_by(INSPECTION.DATE) %>% filter(BORO == "Manhattan"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
    if ( "Bronx" %in% input$borough){
      data = count(ds %>% group_by(INSPECTION.DATE) %>% filter(BORO == "Bronx"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
    if ( "Brooklyn" %in% input$borough){
      data = count(ds %>% group_by(INSPECTION.DATE) %>% filter(BORO == "Brooklyn"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
    if ( "Queens" %in% input$borough){
      data = count(ds %>% group_by(INSPECTION.DATE) %>% filter(BORO == "Queens"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
    if ( "Staten Island" %in% input$borough){
      data = count(ds %>% group_by(INSPECTION.DATE) %>% filter(BORO == "Staten Island"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
  })
  
  cuisine_type_data <- reactive({
    if ( "Korean" %in% input$cursine_type){
      data = count(ds %>% group_by(INSPECTION.DATE) %>% filter(CUISINE.DESCRIPTION == "Korean"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
    if ( "American" %in% input$cursine_type){
      data = count(ds %>% group_by(INSPECTION.DATE) %>% filter(CUISINE.DESCRIPTION == "American"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
    if ( "Chinese" %in% input$cursine_type){
      data = count(ds %>% group_by(INSPECTION.DATE) %>% filter(CUISINE.DESCRIPTION == "Chinese"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
    if ( "Japanese" %in% input$cursine_type){
      data = count(ds %>% group_by(INSPECTION.DATE) %>% filter(CUISINE.DESCRIPTION == "Japanese"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
    if ( "Italian" %in% input$cursine_type){
      data = count(ds %>% group_by(INSPECTION.DATE) %>% filter(CUISINE.DESCRIPTION == "Italian"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
  })
  
  output$Plot3 <- renderPlot({
    data3 = borough_data2()
    print(ggplot(data3,aes(x = INSPECTION.DATE, y = n, group = 1))+geom_line(color = "steelblue")+labs(x="Date",y="Number of Inspections") + labs(title = paste("The relationship between Time and number of Inspections on Restaurants in ", input$borough))+ scale_x_date())
  })
  
  output$Plot4 <- renderPlot({
    data4 = cuisine_type_data()
    print(ggplot(data4,aes(x = INSPECTION.DATE, y = n, group = 1))+geom_line(color = "orange")+labs(x="Date",y="Number of Inspections") + labs(title = paste("The relationship between Time and number of Inspections on ", input$cursine_type, " Restaurants"))+ scale_x_date())
  })
  
  
}

###############################Run App#######################
shinyApp(ui, server)
