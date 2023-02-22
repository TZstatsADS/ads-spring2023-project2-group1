###############################Install Related Packages #######################
if (!require("terra")) {
  install.packages("terra")
  library(terra)
}
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("tidytext")) {
  install.packages("tidytext")
  library(tidytext)
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("wordcloud")) {
  install.packages("wordcloud")
  library(wordcloud)
}
if (!require("stringr")) {
  install.packages("stringr")
  library(stringr)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}
if (!require("lubridate")) {
  install.packages("lubridate")
  library(lubridate)
}

###############################Load The Data #######################

source('global.R')

#Data Prepocessing
year_options = c(2016, 2017,
                 2018, 2019,
                 2020, 2021,
                 2022)

df <- filter(df_unique,inspection_year %in% year_options)
df_no_mod_2022 <- filter(df_no_mod, inspection_year==2022)
df_unique_2022 <- filter(df_unique, inspection_year==2022)
df_barchart <- filter(df_unique,inspection_year %in% c(2019,2020,2021,2022))

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
             sidebarLayout(
               sidebarPanel(
                 h1("About The App"),
                 p("Eating healthy isn't about eating more or eating less, it's about eating right! Our restaurant guide will give you a rundown of what to know and be aware of before visiting restaurants in the city. The information we present is brought to you from inspections conducted by the Department of Health and Mental Hygiene. We care for where you eat!"),
                 p("We'll give you the scoop on the number of restaurants with grade A, B, C, and beyond (because let's face it, sometimes a C is just a fancy way of saying 'gross'), as well as some of the violations that happen across your favourite restaurants."),
                 p("N: This designation is given to restaurants that have violations that are not considered to be a public health hazard."),
                 p("P: This designation is given to restaurants that are in the process of re-opening after being closed for health code violations."),
                 p("Z: This designation is given to restaurants that have not yet been inspected."),
                 p("For more information about grade designations, check out DOHMH's document", tags$a("here", href = "https://www1.nyc.gov/assets/doh/downloads/pdf/rii/blue-book.pdf"), ".")
               ),
               mainPanel(
                 fluidRow(
                   column(width = 6, plotOutput(outputId = "plot1")),
                   column(width = 6, plotOutput(outputId = "plot2"))
                 )
               )
             )
    ),
    
    # Tab 2
    tabPanel("Government Initiatives", value = "Government Initiatives",
             sidebarLayout(
               sidebarPanel(
                 h1("Inspections on Restaurants by borough and Cuisine Type"),
                 p("Recent news has come out that the government is ramping up restaurant inspections in NYC with the city council passing a bill to ensure food delivery apps display accurate health inspection grades. You can see the impact of this policy through the increase in number of inspections in recent years (including some pandemic disruptions)."),
                 selectInput(inputId = "borough", label = "Select borough:",
                             choices = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"),
                             selected = "Manhattan",
                             multiple = TRUE),
                 selectInput(inputId = "cuisine", label = "Select Cuisine Type:",
                             choices = c("Chinese", "American", "Italian", "Japanese","Korean"),
                             selected = "Chinese",
                             multiple = TRUE)
               ),
               mainPanel(
                 plotOutput(outputId = "Plot3"),
                 plotOutput(outputId = "Plot5"),
                 plotOutput(outputId = "Plot4")
               )
             )
    ),
    
 
    # Tab 3
    tabPanel("The Results", value = "The Results",
             sidebarLayout(
               sidebarPanel(
                 h1("Exploring the City's Cuisine with Confidence"),
                 p("Hungry in the city that never sleeps? Now you can make informed decisions about where to eat, hygiene wise. Don't be fooled by those fancy menus and dim lighting – we'll tell you which places have the squeakiest clean kitchens and which to avoid like the plague (or, you know, food poisoning). Whether you're a seasoned foodie or just looking for a quick bite, this page is the perfect companion for any hungry adventurer. Let's explore the city one meal at a time!"),
                 selectInput("boro", "Select borough:", 
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
                   choices = c("A", "B", "C", "P","Z","N"),
                   selected = 'A',
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
                 leafletOutput("map", width = "100%", height = "835px"),
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
      geom_bar(width = 0.7) + 
      scale_x_discrete(labels = function(x) str_wrap(x, width = 200))+
      labs(title="Number of Restaurants by Grade (2022)",x="Grade",y="Count of Restaurant")+
      scale_fill_manual(name = "grade", values=c("#18bc9c","grey")) +
      theme(legend.position = "none",
            axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=16, face="bold"))
  })
  
  #Render Worldcloud
  output$plot2=renderPlot({
    dw = df_no_mod_2022
    
    par(mfrow = c(3, 3), mar = c(1, 1, 1, 1))

    
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
    wordcloud(words = wc_data1$word, freq = wc_data1$n,scale = c(2,0.5),max.words = 10,rot.per = 0, colors="grey50")
    
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
    wordcloud(words = wc_data2$word, freq = wc_data2$n,scale = c(2,0.5),max.words = 10,rot.per = 0, colors="grey50")
    
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
    wordcloud(words = wc_data3$word, freq = wc_data3$n,scale = c(2,0.5),max.words = 10,rot.per = 0, colors="grey50")
    
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
    wordcloud(words = wc_data4$word, freq = wc_data4$n,scale = c(2,0.5),max.words = 10,rot.per = 0, colors="grey50")
    
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
    wordcloud(words = wc_data5$word, freq = wc_data5$n,scale = c(2,0.5),max.words = 10,rot.per = 0, colors="grey50")
    
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
    wordcloud(words = wc_data6$word, freq = wc_data6$n,scale = c(2,0.5),max.words = 10,rot.per = 0, colors="grey50")
    
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
    wordcloud(words = wc_data7$word, freq = wc_data7$n,scale = c(2,0.5),max.words = 10,rot.per = 0, colors="grey50")
    
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
    wordcloud(words = wc_data8$word, freq = wc_data8$n,scale = c(2,0.5),max.words = 10,rot.per = 0, colors="grey50")
    
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
    wordcloud(words = wc_data9$word, freq = wc_data9$n,scale = c(2,0.5),max.words = 10,rot.per = 0, colors="grey50")    
      })
  #Render Barcharts
  
  borough_data3 <- reactive({
    data = df_barchart %>% filter(boro %in% input$borough, cuisine_description %in% input$cuisine)
    data = data %>% group_by(inspection_month) %>% summarise(n = n(), na.rm = TRUE)
    data = data %>% arrange(mdy(data$inspection_month))
    data$inspection_month <- as.Date(data$inspection_month, format="%m/%d/%Y")
    data <- as.data.frame(data)
    return(data)
  })
  
  borough_data4 <- reactive({
    year_grade = df_barchart %>%
      filter(boro %in% input$borough, cuisine_description %in% input$cuisine) %>%
      group_by(inspection_year, grade) %>%
      summarise(n = n(), na.rm = TRUE) %>%
      mutate(inspection_year = as.character(inspection_year))

    t = data.frame(inspection_year = as.character(rep(2018:2023,6)),
                   grade = c(rep('A',6),rep('B',6),rep('C',6),
                             rep('Z',6),rep('P',6),rep('N',6)))

    year_grade = t %>%
      left_join(year_grade, by = c('inspection_year', 'grade')) %>%
      mutate(n = ifelse(is.na(n), 0, n))

    return(year_grade)
  })

  borough_data5 <- reactive({

    year_avg_score = df_barchart %>%
      filter(boro %in% input$borough, cuisine_description %in% input$cuisine) %>%
      group_by(inspection_year, cuisine_description) %>%
      summarise(avg_score = round(mean(score, na.rm=TRUE),2)) %>%
      mutate(inspection_year = factor(inspection_year,
                                      level = c('2018','2019','2020','2021','2022','2023')))

    return(year_avg_score)
  })


  
  output$Plot3 <- renderPlot({
    data3 = borough_data3()
    print(ggplot(data3,aes(x = inspection_month, y = n, group = 1))+
            geom_line(color = "#18bc9c")+
            labs(title = paste("Inspections Over Time"), x="Month",y="Number of Inspections")+ 
            scale_x_date()) +
            theme(legend.position = "none",
            axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=16, face="bold"))
  })
  

  output$Plot4 <- renderPlot({
    data4 = borough_data4()
    print(ggplot(data4,aes(x = inspection_year, y = n ,fill = grade)) +
            geom_bar(stat = 'identity', position = 'fill') +
            labs(x = 'Year',y = 'Share', title = paste('Share of Restaurant Grade')) +
            coord_flip()+
            theme(legend.position="right", 
                  legend.title=element_blank(),
                  axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold"),
                  plot.title = element_text(size=16, face="bold")))
  })

  output$Plot5 <- renderPlot({
    data5 = borough_data5()
    print(ggplot(data5,aes(x = inspection_year, y = avg_score, group = cuisine_description, color=cuisine_description)) +
            geom_line(size = 1) +
            geom_point(size = 2) +
            labs(x = 'Year',y = 'Average Score', title = paste('Average Score of Restaurants (Lower score = Higher Grade)')) +
            theme(legend.position="right", 
                  legend.title=element_blank(),
                  axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold"),
                  plot.title = element_text(size=16, face="bold")))
  })
}

###############################Run App#######################
shinyApp(ui, server)
