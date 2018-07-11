
library(shiny)

shinyUI(
  navbarPage("THR Risk Assessment App",
             tabPanel("Start"),
             tabPanel("Risk Assessments"),
             tabPanel("About Model"),
             tabPanel("Model Selection"),
             tabPanel("Explore Dataset")
  )
  
)