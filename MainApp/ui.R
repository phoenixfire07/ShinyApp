
library(shiny)
library(shinythemes)
library(caret)

shinyUI(
  navbarPage(
            theme = shinytheme("flatly"),
            "THR Risk Assessment App",
             tabPanel("Start"),
            
             tabPanel("Risk Assessment",
                      
                      
                      
                      sidebarPanel(tags$h3("Please enter patient information:"),
                                   sliderInput(inputId = "Age", label = "Select Age", value = 65, min = 30, max=110),
                                   
                                   radioButtons(inputId = "Sex", label="Gender:",
                                                c("Male"=1,
                                                  "Female"=2
                                                )),
                                   
                                   selectInput(inputId = "CIHD", label="Does the patient have a history with CIHD?",
                                               c("No"=0,
                                                 "Yes"=1
                                               )),
                                   
                                   selectInput(inputId = "Hyperthyroidism", label="Does the patient have a history with Hyperthyroidism?",
                                               c("No"=0,
                                                 "Yes"=1
                                               )),
                                   
                                   selectInput(inputId = "Hypothyroidism", label="Does the patient have a history with Hypothyroidism?",
                                               c("No"=0,
                                                 "Yes"=1
                                               )),
                                   
                                   selectInput(inputId = "HT", label="Does the patient have a history with HT?",
                                               c("No"=0,
                                                 "Yes"=1
                                               )),
                                   selectInput(inputId = "AF", label="Does the patient have a history with AF?",
                                               c("No"=0,
                                                 "Yes"=1
                                               )),
                                   
                                   selectInput(inputId = "IDDM", label="Does the patient have a history with IDDM?",
                                               c("No"=0,
                                                 "Yes"=1
                                               )),
                                   
                                   selectInput(inputId = "NIDDM", label="Does the patient have a history with NIDDM?",
                                               c("No"=0,
                                                 "Yes"=1
                                               )),
                                   
                                   selectInput(inputId = "HCD", label="Does the patient have a history with HCD?",
                                               c("No"=0,
                                                 "Yes"=1
                                               )),
                                   
                                   selectInput(inputId = "COPD", label="Does the patient have a history with COPD?",
                                               c("No"=0,
                                                 "Yes"=1
                                               )),
                                   
                                   selectInput(inputId = "Osteoporosis", label="Does the patient have a history with Osteoporosis?",
                                               c("No"=0,
                                                 "Yes"=1
                                               )),
                                   
                                   selectInput(inputId = "HC", label="Does the patient have a history with HC?",
                                               c("No"=0,
                                                 "Yes"=1
                                               )),
                                   
                                   selectInput(inputId = "HCVA", label="Does the patient have a history with HCVA?",
                                               c("No"=0,
                                                 "Yes"=1
                                               )),
                                   
                                   selectInput(inputId = "Dementia", label="Does the patient have a history with Dementia?",
                                               c("No"=0,
                                                 "Yes"=1
                                               )),
                                   
                                   selectInput(inputId = "Alz", label="Does the patient have a history with Alzheimers?",
                                               c("No"=0,
                                                 "Yes"=1
                                               )),
                                   
                                   selectInput(inputId = "DU", label="Does the patient have a history with DU?",
                                               c("No"=0,
                                                 "Yes"=1
                                               )),
                                  
                                   actionButton("button", "Submit"),
                                   textOutput("wait")),

                      mainPanel(
                        tabsetPanel(type="tab",
                        tabPanel("PEin90", tags$h4("90 Day Pulmanory Embolism Risk Profile"), tags$img(src="PEin90.png", height=500, width=900)),
                        tabPanel("RFin30", tags$h4("30 Day Renal Failure Risk Profile"), tags$img(src="RFin30.png", height=500, width=900)),
                        tabPanel("MIin30", tags$h4("30 Day Miocardial Infraction Risk Profile"), tags$img(src="MIin30.png", height=500, width=900)),
                        tabPanel("TIAin30", tags$h4("30 Day Mini Stroke Risk Profile"), tags$img(src="TIAin30.png", height=500, width=900)),
                        tabPanel("CDiff", tags$h4("Clostridium Difficile Risk Profile"), tags$img(src="CDiff.png", height=500, width=900)),
                        tabPanel("Death90", tags$h4("90 Day Mortality Risk Profile"), tags$img(src="NNdied90.png", height=500, width=900)),
                        tabPanel("CIin30", tags$h4("30 Day Chest Infection Risk Profile"), tags$img(src="CIin30.png", height=500, width=900))
                        
                        ))
                      
                      
                      #end of tab
                      ),

                      
                      
                      
             tabPanel("About Model"),
             tabPanel("Model Selection"),
             tabPanel("Explore Dataset")
  )
  
  
  
)