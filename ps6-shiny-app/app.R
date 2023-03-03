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

#Data Cleaning
sample_n(base, 5)

colnames(base)

shopping <- base %>% 
  group_by(`Segment Type`, `Segment Description`)
head(shopping, 10)

shopping_gender <- shopping %>% 
  filter(`Segment Type` == "Gender")
shopping_gender

# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    ## TAB 1
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
    
    ## TAB 2
    tabPanel("Plots",
             titlePanel("Frequency of Shoppers According to Social Media"),
             p("Here you can see the frequency of users who spend by their
               gender identity. You may also choose the color of the graph!"),
             
             mainPanel(plotOutput("barplot")),
             
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

    ## TAB 3
    tabPanel("Tables",
          
             ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## ABOUT OUR DATA PAGE
  output$sample <- renderTable(
    base %>% 
      sample_n(5)
    )
 
  ## PLOTS
  output$barplot <- renderPlot({
    shopping_gender %>%
    filter(input$gender == "Both" | `Segment Description` == input$gender) %>%
      ggplot() +
      geom_bar(mapping = aes(x = Answer, y = Count), 
               stat = 'identity',
               fill = input$color) +
      labs(
        x = "Social Media",
        y = "Number of Shoppers"
      )
      
  })
    
  
  
  ## TABLES
}

# Run the application 
shinyApp(ui = ui, server = server)
