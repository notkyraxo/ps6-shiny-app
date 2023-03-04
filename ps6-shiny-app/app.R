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
library(dplyr)

# Load Data
base <- read_delim("WhatsgoodlyData-6.csv") #unedited/raw data

# DATA CLEANING
sample_n(base, 5)

colnames(base)

shopping <- base %>% 
  group_by(`Segment Type`, `Segment Description`)
head(shopping, 10)

## Subset for Plot Tab
shopping_gender <- shopping %>% 
  filter(`Segment Type` == "Gender")
shopping_gender

shopping_uni <- shopping %>% 
  filter(`Segment Type` == "University")
shopping_uni

# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    ## TAB 1 - ABOUT DATA
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
    
    ## TAB 2 - PLOTS
    tabPanel("Plots",
             titlePanel("Frequency of Shoppers According to Social Media"),
             p("Here you can see the frequency of users who spend by their
               gender identity. You can also choose the color of the graph!"),
             
             mainPanel(plotOutput("barplot"),
                       textOutput("sentence1")
                       ),
             
             sidebarPanel(
               fluidRow(
                 column(6,
                        radioButtons("color", "Choose color:",
                                     choices = c("purple3", "pink2", "lightgreen",
                                                  "skyblue"))),
                 column(6,
                        radioButtons("gender", "Choose gender:",
                                     choices = c(unique(shopping_gender$`Segment Description`), 
                                                   "Both")))
                 )
               )
             ),

    ## TAB 3 - TABLES
    tabPanel("Tables",
             titlePanel("Percentage of Shoppers According to University"),
             p("Here you can look at a table that gives the different frequencies
                of social media platforms according to College/University."),
             
             mainPanel(tableOutput("table"),
                       textOutput("sentence2")
                       ),
          
             sidebarPanel(
               fluidRow(
                 column(6,
                        radioButtons("university", "Choose university:",
                                     choices = c(unique(shopping_uni$`Segment Description`))))
                  )
                )
              )
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## ABOUT OUR DATA PAGE
  output$sample <- renderTable(
    base %>% 
      sample_n(5))
 
  ## PLOTS
  output$barplot <- renderPlot({
    shopping_gender %>%
    filter(input$gender == "Both" | `Segment Description` == input$gender) %>%
      ggplot() +
      geom_bar(mapping = aes(x = Answer, y = Count), 
               stat = 'identity',
               fill = input$color) +
      labs(x = "Social Media", y = "Number of Shoppers")
  })
  
  output$sentence1 <- renderText({
    "(max count) was the most popular platform for female/male shoppers!"
  })
    
  ## TABLES
  output$table <- renderTable({
    shopping_uni %>% 
      select(`Segment Description`, Answer, Percentage) %>% 
      filter(`Segment Description` == input$university)
  })
  
  output$sentence2 <- renderText({
    "(insert app with max percentage(s) here) was the most
     popular at (input$university)!"
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
