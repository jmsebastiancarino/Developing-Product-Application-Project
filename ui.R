# Loading all the necessary and relevant packages
library(shiny)
library(ggplot2)
library(datasets)
library(shinythemes)


# Setting up the page 
shinyUI(fluidPage(
        
        # Setting up the background and font theme 
        theme = shinytheme("slate"),
  
        # Application title
        titlePanel("Interactive Data Science"),
        
        
        # Sidebar where the choices are located
        sidebarPanel(
          uiOutput("first"),
          uiOutput("second"),
          uiOutput("third"),
          
        # Shows the date, time, and remarks outputs
        
          p(strong(h5(uiOutput("Date")))),
          p(strong(h5(uiOutput("Time")))),
          p(strong(h5(uiOutput("Remarks"))))
                     ),
    
        # Shows the introduction and output results
        mainPanel(
               uiOutput("Introduction"),
               tabsetPanel(type = "pills",
                           tabPanel("Data", verbatimTextOutput("Data")),
                           tabPanel("Summary", verbatimTextOutput("summary")),
                           tabPanel("Plot", plotOutput("plot"))
                           
          )
    )
  ))

