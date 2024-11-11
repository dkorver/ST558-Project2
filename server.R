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
library(gtsummary)
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
    
    # Create separate summaries for numeric and character columns
    summary_list <- list()
    
    # For numeric columns: classical statistics
    num_cols <- sapply(datasel, is.numeric)
    if (any(num_cols)) {
      num_summary <- summary(datasel[, num_cols, drop = FALSE])
      summary_list$numeric <- num_summary
    }
    
    # For character/factor columns: frequency tables
    char_cols <- sapply(datasel, function(x) is.character(x) | is.factor(x))
    if (any(char_cols)) {
      char_summary <- lapply(datasel[, char_cols, drop = FALSE], function(x) {
        freq_table <- table(x)
        prop_table <- prop.table(freq_table) * 100
        data.frame(
          Frequency = as.numeric(freq_table),
          Percentage = round(as.numeric(prop_table), 2)
        )
      })
      summary_list$categorical <- char_summary
    }
    
    # Print the results
    if (length(summary_list$numeric) > 0) {
      cat("Numerical Variables:\n")
      cat("===================\n")
      print(summary_list$numeric)
      cat("\n")
    }
    
    if (length(summary_list$categorical) > 0) {
      cat("\nCategorical Variables:\n")
      cat("=====================\n")
      for (var_name in names(summary_list$categorical)) {
        cat("\n", var_name, ":\n", sep = "")
        print(summary_list$categorical[[var_name]])
      }
    }
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

