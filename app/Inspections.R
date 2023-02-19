if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("lubridate")) {
  install.packages("lubridate")
  library(lubridate)
}

data = read.csv("/Users/haoyuhe/Documents/GitHub/ads-spring2023-project2-group1/data/DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
data = data %>% filter(grepl("2020", data$INSPECTION.DATE) | grepl("2019", data$INSPECTION.DATE) | grepl("2021", data$INSPECTION.DATE))
data = data %>% filter(data$CUISINE.DESCRIPTION =="Chinese"|data$CUISINE.DESCRIPTION =="Korean"|data$CUISINE.DESCRIPTION =="Japanese"|data$CUISINE.DESCRIPTION =="American"|data$CUISINE.DESCRIPTION =="Italian") %>% group_by(BORO) %>% select(BORO,INSPECTION.DATE,CUISINE.DESCRIPTION)
data1 <- count(data %>% group_by(INSPECTION.DATE) %>% filter(CUISINE.DESCRIPTION == "Korean"))
data1 = data1 %>% arrange(mdy(data1$INSPECTION.DATE))
data1$INSPECTION.DATE <- as.Date(data1$INSPECTION.DATE, format="%m/%d/%Y")
print(ggplot(data1,aes(x = data1$INSPECTION.DATE, y = data1$n, group = 1))+geom_line(color = "steelblue")+labs(x="Date",y="Number of Inspections") + labs(title = paste("The relationship between Time and number of Inspections on Restaurants in ")) + scale_x_date())

ui <- fluidPage(
  titlePanel("Inspections on Restaurants by borough and cursine type"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "borough", label = "Choose a borough:",
                  choices = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")),
      selectInput(inputId = "cursine_type", label = "Choose a cursine type:",
                  choices = c("Chinese", "American", "Italian", "Japanese","Korean")),
    ),
    mainPanel(
      plotOutput(outputId = "Plot1"),
      plotOutput(outputId = "Plot2")
    )
  )
)

server <- function(input, output) {
  
  borough_data <- reactive({
    if ( "Manhattan" %in% input$borough){
      data = count(data %>% group_by(INSPECTION.DATE) %>% filter(BORO == "Manhattan"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
    if ( "Bronx" %in% input$borough){
      data = count(data %>% group_by(INSPECTION.DATE) %>% filter(BORO == "Bronx"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
    if ( "Brooklyn" %in% input$borough){
      data = count(data %>% group_by(INSPECTION.DATE) %>% filter(BORO == "Brooklyn"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
    if ( "Queens" %in% input$borough){
      data = count(data %>% group_by(INSPECTION.DATE) %>% filter(BORO == "Queens"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
    if ( "Staten Island" %in% input$borough){
      data = count(data %>% group_by(INSPECTION.DATE) %>% filter(BORO == "Staten Island"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
  })
  
  cuisine_type_data <- reactive({
    if ( "Korean" %in% input$cursine_type){
      data = count(data %>% group_by(INSPECTION.DATE) %>% filter(CUISINE.DESCRIPTION == "Korean"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
    if ( "American" %in% input$cursine_type){
      data = count(data %>% group_by(INSPECTION.DATE) %>% filter(CUISINE.DESCRIPTION == "American"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
    if ( "Chinese" %in% input$cursine_type){
      data = count(data %>% group_by(INSPECTION.DATE) %>% filter(CUISINE.DESCRIPTION == "Chinese"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
    if ( "Japanese" %in% input$cursine_type){
      data = count(data %>% group_by(INSPECTION.DATE) %>% filter(CUISINE.DESCRIPTION == "Japanese"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
    if ( "Italian" %in% input$cursine_type){
      data = count(data %>% group_by(INSPECTION.DATE) %>% filter(CUISINE.DESCRIPTION == "Italian"))
      data = data %>% arrange(mdy(data$INSPECTION.DATE))
      data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
      return(data)
    }
  })
  

  output$Plot1 <- renderPlot({
    data = borough_data()
    print(ggplot(data,aes(x = INSPECTION.DATE, y = n, group = 1))+geom_line(color = "steelblue")+labs(x="Date",y="Number of Inspections") + labs(title = paste("The relationship between Time and number of Inspections on Restaurants in ", input$borough))+ scale_x_date())
  })
  
  output$Plot2 <- renderPlot({
    data = cuisine_type_data()
    print(ggplot(data,aes(x = INSPECTION.DATE, y = n, group = 1))+geom_line(color = "orange")+labs(x="Date",y="Number of Inspections") + labs(title = paste("The relationship between Time and number of Inspections on ", input$cursine_type, " Restaurants"))+ scale_x_date())
  })
}

shinyApp(ui = ui, server = server)
