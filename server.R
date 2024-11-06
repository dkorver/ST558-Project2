#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(readr)
library(DT)
library(rsconnect)
nfl_plays <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  InputDataset <- reactive({
    nfl_plays
  })
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "nfl" = nfl_plays,
    )
  }, ignoreNULL = FALSE)
  
  # Generate a list of x-axis variables ----
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    newdata <- nfl_plays %>% select(input$xcol,input$ycol,input$byvar)
    newdata
  })
  
  #Generate graph based on user input
  output$barPlot <- renderPlot({
    #get data
    nflData <- selectedData()
    
    ggplot(nflData, aes(x=input$xcol, y=input$ycol, color=as.factor(input$byvar))) + geom_point(shape=1)
  })
  
  output$summaryList <- renderPrint({
    datasel <- selectedData()
    summary(datasel)
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- InputDataset()
    summary(dataset)
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(InputDataset(), file, row.names = FALSE)
    }
  )
  
  output$ex1 <- DT::renderDataTable(
    DT::datatable(InputDataset(), options = list(pageLength = 5))
  )
  
}

