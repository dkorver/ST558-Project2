#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readr)
library(rsconnect)

nfl_plays <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")
vars <- setdiff(names(nfl_plays), "nfl")

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
    "Project 2",
    id = "main_navbar",
    tabPanel("About",
             br(),
             h4("My First R Shiny App!"),
             div(img(src = "nfl_logo.png"),style = "text-align: left;"),
             "Image Source: nfl.com", 
             br(),
             br(),
             "This R Shiny App is using the data set from the", a(href = "https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016/data", " Detailed NFL Play-by-Play Data 2009-2018"),"from Kaggle.com.  
  There are two different pages in this app.    
  There is a 'Data Exploration' page where you can look at the first few observations in the dataset and summary statistics of all the variables.  
  There is a 'Data Download' page where you can download and view a subset of the dataset.",
             br(),
             br(),
    ),  
    
    tabPanel("Data Exploration",
             # App title ----
             titlePanel("NFL play-by-play Data"),
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               # Sidebar panel for inputs ----
               sidebarPanel(
                 # Input: Select a dataset ----
                 selectInput("dataset", "Choose a dataset:",
                             choices = c("nfl")),
                 br(),
                 h4("Choose variables to create some classical stats and graph them"),
                 selectInput('xcol', 'X Variable', vars),
                 selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
                 selectInput('byvar', 'Classification Variable', vars, selected = vars[[3]]),
                 br(),
               ),
               # Main panel for displaying outputs ----
               mainPanel(
                 h4("Graph of selected variables"),
                 plotOutput("barPlot"),
                 # Output: Header + summary of distribution ----
                 h4("Summary of selected varibles"),
                 verbatimTextOutput("summaryList"),
               )
               
             )
    ),
    
    tabPanel("Data Download",
             # App title ----
             titlePanel("Downloading Data"),
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               # Sidebar panel for inputs ----
               sidebarPanel(
                 # Input: Choose dataset ----
                 selectInput("dataset", "Choose a dataset:",
                             choices = c("nfl")),
                 # Button
                 downloadButton("downloadData", "Download")
               ),
               # Main panel for displaying outputs ----
               mainPanel(
                 # Output: Header + table of distribution ----
                 h4("Observations"),
                 DT::dataTableOutput('ex1')
               )
             )
    )
    
  )
)
