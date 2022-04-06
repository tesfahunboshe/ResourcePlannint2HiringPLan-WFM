library(shiny)
library(tidyverse)
library(DT)
library(shinydashboard)
library(shinythemes)
# Define UI for data upload app ----
ui <- fluidPage(theme = shinytheme("spacelab"),
  
  # App title ----
  titlePanel("Settings"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(width = 3,

      # Input: Select a file ----
      fileInput("file1", "FTEs File: (csv)",
                multiple = FALSE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),

      # Horizontal line ----
      tags$hr(),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),

      # Input: Select quotes ----
      numericInput("shiftlength", 
                   "Shift Length:", value=8, min = 1, max = 24),
      numericInput("weeklydays", 
                   "Weekly working days:", value=5, min = 1, max = 7),

      tags$hr(),
      textInput("eff_required", "Required Efficiency %:", value = "80 %"),

    ),

    # Main panel for displaying outputs -----
    mainPanel(

      # Output: Data files ----
      fluidRow(
        column(6,
               h2("FTE's Per Interval"),
               dataTableOutput("capplan")),
        
        column(6,
               h2("Schedule Shells"),
               dataTableOutput("scheduleshells")),
        
      ),
      # Output: Coverage metrics and plot
      fluidRow(
        column(6,
               h2("Heads Count"),
               h2(textOutput("HeadsCOunt"))),
        column(6,
               h2("Efficiency"),
               h2(textOutput("Efficiency")))),
      
      fluidRow(box(
          
          fluidRow(plotOutput('coverageplots')),
          collapsible = TRUE, width = 12, solidHeader = TRUE, status = "primary", title = "Coverage vs Requirement")
        
      )
      

    )

  )
)
