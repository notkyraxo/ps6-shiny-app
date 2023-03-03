#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)

# Load Data
shopping <- read_delim("WhatsgoodlyData-6.csv")

#Data Cleaning
sample_n(shopping, 5)

colnames(shopping)

shopping2 <- shopping %>% 
  group_by(`Segment Type`, `Segment Description`)
head(shopping2, 10)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    tabPanel("About Our Data",
             titlePanel("Shopping n' Social Media"),
             
             h1("Summary"),
             p("This report provides a broad overview of how online advertising
               on",
                strong("social media affects consumers and their shopping habits."),
               "This data primarily looks at", 
                em("Millenials and Gen Z members"), 
               "and their activity on apps like Facebook, Twitter, Instagram, 
               and Snapchat."),
             
             h1("Sample"),
             p("This dataset contains 300,000 observations and 6 variables.
               Here is a small (random) sample of the data below."),
             tableOutput("sample")
             ),
    
    tabPanel("Plots",
             titlePanel("Frequency of Shoppers According to Social Media"),
             p("Here you can see the frequency of users who spend by their
               gender identity."),
             
             mainPanel(plotOutput("barplot")),
             
             sidebarPanel(
               fluidRow(
                 column(6,
                        radioButtons("Color", "Choose color:",
                                     choices = c("purple3", "pink2", "lightgreen",
                                                  "skyblue"))
                        )
                 )
               )
             )
             
             ),
    
    tabPanel("Tables",
          
             ),
    
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## ABOUT OUR DATA PAGE
  output$sample <- renderTable(
    shopping %>% 
      sample_n(5)
    )
 
  ## PLOTS
  output$barplot <- renderPlot({
      shopping2 %>%
      filter(`Segment Type` == "Gender") %>%
      ggplot() +
      geom_bar(mapping = aes(x = Answer, y = Count), 
               stat = 'identity',
               fill = "blue")
  })
    
  
  
  ## TABLES
}

# Run the application 
shinyApp(ui = ui, server = server)
