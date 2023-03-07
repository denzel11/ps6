library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)

mobility <- read.delim("/Users/apple/documents/Github/shinyapp1/download_data.csv", sep=',')
mobility$Date <- as.Date(mobility$Date)
mobility$Province <- as.factor(mobility$Province)

ui <- fluidPage(
    titlePanel("my app"),
    
    sidebarLayout(
        
        sidebarPanel(
            p("This is the sidebar"),
            selectInput(inputId = "dv", label = "Category",
                        choices = c("Retail_Recreation", "Grocery_Pharmarcy", "Parks", "Transit_Stations", "Workplaces", "Residential"),
                        selected = "Grocery_Pharmarcy"),
            checkboxGroupInput(inputId = "provinces", "Province(s)",
                        choices = levels(mobility$Province),
                        selected = levels(mobility$Province)),
            dateRangeInput(inputId = "date", "Date range",
                           start = min(mobility$Date),
                           end   = max(mobility$Date)),
        ),
        
        mainPanel(
            
            tabsetPanel(
                tabPanel("Plot", plotOutput("plot")),
                tabPanel("Summary", 
                         h1("this data contains 1014 observations and 9 variables."), 
                         h2("here is a small sample of the dataset."),
                         tableOutput("sample")),
                tabPanel("Table", dataTableOutput("table"))
            )
        )
        
    )
)
      
       


server <- function(input, output) {
    output$sample <- renderTable({
        mobility %>%
            sample_n(6)
    })
    filtered_data <- reactive({
        subset(mobility,
               Province %in% input$provinces &
                   Date >= input$date[1] & Date <= input$date[2])})
    
    output$plot <- renderPlot({
      mobility %>%
        filter(Province %in% input$provinces) %>%
        ggplot(aes(x=Date, y=Residential))+
          geom_point(aes(col=Province)) +
          geom_line(aes(col=Province))
    })
    
    output$table <- renderDataTable({
        filtered_data()
    })
}

shinyApp(ui = ui, server = server)