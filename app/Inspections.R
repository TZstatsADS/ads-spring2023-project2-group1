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


data = read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv")

data_filter = data %>% 
  filter(!is.na(SCORE), GRADE != '') %>% 
  mutate(INSPECTION.DATE = as.Date(INSPECTION.DATE, format="%m/%d/%Y"),
         INSPECTION.YEAR = year(INSPECTION.DATE),
         GRADE.DATE = as.Date(GRADE.DATE, format="%m/%d/%Y"),
         GRADE.YEAR = year(GRADE.DATE)) %>% 
  filter(INSPECTION.YEAR >= 2018) %>% 
  group_by(CAMIS) %>% 
  filter(row_number(desc(GRADE.DATE)) == 1) %>% 
  ungroup()

borough_list = data_filter %>%
  count(BORO) %>% 
  arrange(desc(n)) %>% 
  filter(BORO != '0') %>% 
  head(10) %>% 
  pull(BORO)
  
cuisine_list = data_filter %>%
  count(CUISINE.DESCRIPTION) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  pull(CUISINE.DESCRIPTION)



ui <- fluidPage(
  titlePanel("Inspections on Restaurants by borough and cursine type"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "borough", label = "Choose a borough:",
                  choices = borough_list),
      selectInput(inputId = "cursine_type", label = "Choose a cursine type:",
                  choices = cuisine_list),
    ),
    mainPanel(
      fluidRow(
        column(6,plotOutput(outputId = "Plot1")),
        column(6,plotOutput(outputId = "Plot2")),
        column(6,plotOutput(outputId = "Plot3")),
        column(6,plotOutput(outputId = "Plot4"))
     )
  )
))

server <- function(input, output) {
  
  borough_data <- reactive({
    
    year_grade = data_filter %>% 
      filter(BORO %in% input$borough) %>% 
      group_by(INSPECTION.YEAR, GRADE) %>% 
      summarise(n = n()) %>% 
      mutate(INSPECTION.YEAR = as.character(INSPECTION.YEAR))
    
    t = data.frame(INSPECTION.YEAR = as.character(rep(2018:2023,6)),
                   GRADE = c(rep('A',6),rep('B',6),rep('C',6),
                             rep('Z',6),rep('P',6),rep('N',6)))
    
    year_grade = t %>% 
      left_join(year_grade, by = c('INSPECTION.YEAR', 'GRADE')) %>% 
      mutate(n = ifelse(is.na(n), 0, n))
    
    return(year_grade)

  })


  cuisine_type_data <- reactive({
    
    year_grade = data_filter %>% 
      filter(CUISINE.DESCRIPTION %in% input$cursine_type) %>% 
      group_by(INSPECTION.YEAR, GRADE) %>% 
      summarise(n = n()) %>% 
      mutate(INSPECTION.YEAR = as.character(INSPECTION.YEAR))
    
    t = data.frame(INSPECTION.YEAR = as.character(rep(2018:2023,6)),
                   GRADE = c(rep('A',6),rep('B',6),rep('C',6),
                             rep('Z',6),rep('P',6),rep('N',6)))
    
    year_grade = t %>% 
      left_join(year_grade, by = c('INSPECTION.YEAR', 'GRADE')) %>% 
      mutate(n = ifelse(is.na(n), 0, n))
    
    return(year_grade)

  })
  
  borough_score = reactive({
    
    year_avg_score = data_filter %>% 
      filter(BORO %in% input$borough) %>% 
      filter(CUISINE.DESCRIPTION %in% cuisine_list) %>% 
      group_by(INSPECTION.YEAR, CUISINE.DESCRIPTION) %>% 
      summarise(avg_score = round(mean(SCORE),2)) %>% 
      mutate(INSPECTION.YEAR = factor(INSPECTION.YEAR, 
                                      level = c('2018','2019','2020','2021','2022','2023')))
    
    return(year_avg_score)
  })
  
  cuisine_score = reactive({
    
    year_avg_score = data_filter %>% 
      filter(CUISINE.DESCRIPTION %in% input$cursine_type) %>%
      filter(BORO %in% borough_list) %>% 
      group_by(INSPECTION.YEAR, BORO) %>% 
      summarise(avg_score = round(mean(SCORE),2)) %>% 
      mutate(INSPECTION.YEAR = factor(INSPECTION.YEAR, 
                                      level = c('2018','2019','2020','2021','2022','2023')))
    
    return(year_avg_score)
    
  })

output$Plot1 <- renderPlot({
  data = borough_data()
  print(data %>% 
          ggplot(aes(x = INSPECTION.YEAR, y = n ,fill = GRADE)) + 
          geom_bar(stat = 'identity', position = 'fill') + 
          labs(x = 'Year',y = 'Prob', title = paste0('Grade of Restaurants in Year(', input$borough,')')) + 
          coord_flip()+
          theme(legend.position="top", legend.title=element_blank()))
})

output$Plot2 <- renderPlot({
  data = cuisine_type_data()
  print(data %>% 
          ggplot(aes(x = INSPECTION.YEAR, y = n ,fill = GRADE)) + 
          geom_bar(stat = 'identity', position = 'fill') + 
          labs(x = 'Year',y = 'Prob', title = paste0('Grade of Restaurants in Year(', input$cursine_type, ')')) + 
          coord_flip()+
          theme(legend.position="top", legend.title=element_blank()))
  })

output$Plot3 <- renderPlot({
  data = borough_score()
  print(data %>% 
          ggplot(aes(x = INSPECTION.YEAR, y = avg_score, group = CUISINE.DESCRIPTION, color=CUISINE.DESCRIPTION)) +
          geom_line(size = 1) + 
          geom_point(size = 2) +
          labs(x = 'Year',y = 'Avg Score', title = paste0('Avg Score of Restaurants in Year(', input$borough, ')')) +
          theme(legend.position="top", legend.title=element_blank())
          )
})

output$Plot4 <- renderPlot({
  data = cuisine_score()
  print(data %>%
          ggplot(aes(x = INSPECTION.YEAR, y = avg_score, group = BORO, color=BORO)) +
          geom_line(size = 1) + 
          geom_point(size = 2) +
          labs(x = 'Year',y = 'Avg Score', title = paste0('Avg Score of Restaurants in Year(', input$cursine_type,')'))+
          theme(legend.position="top", legend.title=element_blank()))
})

}


shinyApp(ui = ui, server = server)



