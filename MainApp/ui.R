
library(shiny)
library(shinythemes)
library(caret)


# About the model--------------------------------

AboutNN <-"A single hidden layer neural network was selected as the model for the THR Risk Assessment App. The model performed relatively well
when compared to alternatives for two important parameter: Accuracy and Sensitivity. For a detailed comparison to other models, please see the Model Selection section.
When using a tool based on a predictive model, it is important to understand the overall reliability of the model. As such, a detailed description 
of the Accuracy, Sensitivity and Specificty of the model for each complication is provided."

AboutNNTrain<- "All neural networks were trained on a dataset of over 400,000 THR cases including patient demographics and comorbidies.
Based on this patient data, the models were then tested on a dataset over 150,000 patient records in order to estimate patient risk. The results
of this test are shown here."

aboutRisk<- 
  "This risk profile has been estimated using a neural networks trained on a dataset of over 400,000 THR surgery instances. 
  Patient demographics and comorbidies have been weighted using backpropogation ensuring high overall accuracy and identification of 
  high-risk cases. The single hidden layer neural network below was developed specifically for this complication using the neuralnet package
  and is the basis of the risk estimations provided. To learn more about the model, please see the About Model section."

riskLevels<-
  "A LOW risk outcome indicates that the neural network estimates a patient's probablity of having a complication to be
between 0-25%. A MODERATE risk outcome indicates that the neural network estimates a patient's probablity of having a complication to be
between 26-50%. A HIGHER risk outcome indicates that the neural network estimates a patient's probablity of having a complication to be
between 50-75%. A HIGHEST risk outcome indicates that the neural network estimates a patient's probablity of having a complication to be
between 75-100%. All risk estimates are based on a patients similarity to those who have suffered from compliation in the past."
  
exploringModel<- "The THR Risk Assessment model was trained and tested on a dataset of over 500,000 THR episodes recorded by Hospital Episode
  Statistics (HES), which provides access to inpatient data on all joint replacements performed in the English National Health Service (NHS).
  Overall, the percentage of surgeries that result in complications are under 2%. In order to create a model that would accurately detect a majority
of episodes resulting in complications, resampling was done using SMOTE in order to oversample cases in which complications occurred post surgery. While this
can marginally decrease the overall accuracy of the model, it ensures that the model can identify positive complication outcomes more readily."

aboutCM<- "After the neural network model is trained on a subset of the original dataset, it is tested on a test set. The results of
these test can provide information on how well the model predicts the likelyhood of complications. When the model runs through the test set,
it makes a guess at whether a particular episode resulted in a complication given the demographics and comorbidies of a patient. When it has run
through all of the episode, a confusion matrix is created. This indicates the number of true Positive (TP) and trues negatives (TN) produced, as well as the number
of false positives and false negatives.  To assess the model for each complication, three key charachteristics are analysed: Accuracy, Specificity, and Sensitivity."
  
accuracy<- "Classification accuracy is defined as the number of correct predictions out of the total number of predictions (TP+TN/Total).
We aim to maintain an accuracy no lower than 75%."

specificity<- "Classification specificity is also known as the true negative rate. This is the number of true negatives identified out of 
the total number of true negatives in the test set (TN/Total Negatives). Specificty rates below 75% were not acceptable for these models."

sensitivity<- "Classification sensitivity is perhaps the most important charchteristic for the THR Risk Assessment App. This 
is known as the true positive rate, identifying the true positive predictions out of the total number of positive outcomes in the test set (TP/Total Positive)
This parameter is not usually predicted as well as accuracy or specificty because it requires idenitifying unique traits in patients that
had a complication. Many complications tested resulted in extremely low true positive rates and therefore, only those with sensitivities above 
40% were used in the THR Risk Assessment App"

aboutTHRbackground<-
  "Total Hip Replacement (THR) is a highly successful and cost-effective intervention with over 75,000 procedures performed a year. Major complications occur in 2% 
of procedures with the majority occurring in the first four post-operative days. Hospital episode statistics (HES) provides access to inpatient data on all joint 
replacements performed in the English National Health Service (NHS). Novel methods of analysing this data can allow clinicians to plan intervention in the safest environment 
based on bespoke peri- and post-operative care pathways. This has the potential to reduce morbidity and mortality, reduce healthcare costs through improved efficiency and safe care, 
and maximise benefit from THR. "

appObjective<-
  "The aim of this tool is to create a web-based application for calculating risk of complications following hip replacement. 
Entry of simple patient demographics and co-morbidity data will produce an estimated risk profile for a range of  complications, and will enable the clinician to tailor peri- and post-operative 
needs, and allow  bespoke, ‘patient-specific’ consent."

riskAssessment <-
  "The 'Risk Assessment' tool can be use to obtain a risk profile for several different complications based on patient demographics
and comorbidities. To use this tool, simply enter patient information in the information panel and click 'Estimate Risk' in each 
complication tab to generate a risk profile for each complication."

exploreModel<-
  "In order to understand the underlying predictive model from which risk profile's are derived, further information regarding the characteristics of the neural networks (NNs) used 
as well as a comparison between NNs and other predictive modeling methods can be found in the 'About Model' and 'Model Selection' sections of the App."
  
shinyUI(
  navbarPage(
            theme = shinytheme("cosmo"),
            "THR Risk Assessment App",
             tabPanel("Start",
                      fluidPage(
                        tags$h1(tags$b(div(style="text-align:center;","Welcome to the Total Hip Replacement Risk Assessment Application")),tags$br()),
                        div(
                          style=
                            "width:75% ; 
                            height:380px; 
                            background-image: url(THR.jpg);
                            background-size:cover;
                            background-repeat: no-repeat;
                            margin: auto;
                            "),
                       
                        tags$h3(tags$b("Background information:")),
                        tags$h4(aboutTHRbackground),
                        tags$br()
                        ),
                      fluidPage(
                      sidebarPanel(
                        radioButtons(inputId = "yearlyStats", label="Please select a complication to view number of yearly episodes as recorded by the HES from 2005 to 2014:",
                  
                                    c("Total THR Surgeries"="yearlyTotal",
                                      "Transient Ischemic Attack"="yearlyTIA",
                                      "Myocardial infarction"="yearlyMI",
                                      "Renal Failure"="yearlyRF",
                                      "Clostridium Difficile "="yearlyCDiff",
                                      "90 Day Mortality"="yearlyd90",
                                      "Chest Infection"="yearlyCI",
                                      "Infection"="yearlyInfection"))
                      ),
                      mainPanel(
                        plotOutput("yearlyStatsPlot")
                        
                        )
                      ),
                      tags$br(),
                      fluidPage(
                        tags$h3(tags$b("Objective of this tool:")),
                        tags$h4(appObjective),
                        tags$br(),
                        tags$h3(tags$b("How to use the THR Risk Assessment App:")),
                        column(6,
                          tags$h4(tags$b("Risk Assessment:")),
                          tags$h4(riskAssessment)),
                        column(6, 
                          tags$h4(tags$b("Explore Model:")),
                          tags$h4(exploreModel),
                          tags$br(),
                          tags$br(),
                          tags$br()),

                        fluidPage(
                          column(6, 
                                 div(
                                    style=
                                    "display: block;
                                     margin-left: auto;
                                     margin-right: auto;
                                     width: 50%;",
                                  tags$img(src="ucl.png", height=90, width=270)
                                  ),
                                 tags$br(),
                                 tags$br() 
                                 ),
                          column(6,
                                 div(
                                   style=
                                     "display: block;
                                      margin-left: auto;
                                      margin-right: auto;
                                      width: 50%;",
                                 tags$img(src="nhs.png", height=90, width=210)
                                 ),
                                 tags$br(),
                                 tags$br()
                                 )
                        )
                      )),
            
             tabPanel("Risk Assessment",
                      
                      
                      
                      sidebarPanel(tags$h3("Please enter patient information:"),
                                   sliderInput(inputId = "Age", label = "Select Age", value = 65, min = 30, max=110),
                                   
                                   radioButtons(inputId = "Sex", label="Gender:",
                                                c("Male"=.1111111111,
                                                  "Female"=.222222222
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
                                               ))
                                  
                                
                                   ),

                      mainPanel(
                        tabsetPanel(type="tab",
                                    
                       
                                    
                        tabPanel("RFin30", 
                                 tags$h2("Estimated 30 Day Renal Failure Risk Profile"),
                                 tags$h6("Please enter patient information and click 'Esitmate Risk' to generate  a risk profile for this complication."),
                                 actionButton("RFbtn", "Estimate Risk"),
                                 tags$h3(textOutput("RF")),
                                 tags$br(),
                                 tags$h5(tags$b("About this model:")),
                                 tags$h5(aboutRisk),
                                 tags$h5(riskLevels),
                                 plotOutput("RFRiskPlot")
                                 ),
                        
                        
                        tabPanel("MIin30",
                                 tags$h2("30 Day Myocardial Infarction Risk Profile"), 
                                 tags$h6("Please enter patient information and click 'Esitmate Risk' to generate  a risk profile for this complication."),
                                 actionButton("MIbtn", "Estimate Risk"),
                                 tags$h3(textOutput("MI")),
                                 tags$br(),
                                 tags$h5(tags$b("About this model:")),
                                 tags$h5(aboutRisk),
                                 tags$h5(riskLevels),
                                 plotOutput("MIRiskPlot")
                                 ),
                        
                        
                        tabPanel("TIAin30", 
                                 tags$h2("30 Day Transient Ischemic Attack Risk Profile"), 
                                 tags$h6("Please enter patient information and click 'Esitmate Risk' to generate  a risk profile for this complication."),
                                 actionButton("TIAbtn", "Estimate Risk"),
                                 tags$h3(textOutput("TIA")),
                                 tags$br(),
                                 tags$h5(tags$b("About this model:")),
                                 tags$h5(aboutRisk),
                                 tags$h5(riskLevels),
                                 plotOutput("TIARiskPlot")
                                 ),
                        
                        
                        tabPanel("CDiff",
                                 tags$h2("Clostridium Difficile Risk Profile"), 
                                 tags$h6("Please enter patient information and click 'Esitmate Risk' to generate  a risk profile for this complication."),
                                 actionButton("CDiffbtn", "Estimate Risk"),
                                 tags$h3(textOutput("CDiff")),
                                 tags$br(),
                                 tags$h5(tags$b("About this model:")),
                                 tags$h5(aboutRisk),
                                 tags$h5(riskLevels),
                                 plotOutput("CDiffRiskPlot")
                                 ),
                        
                        
                        tabPanel("Death90", 
                                 tags$h2("90 Day Mortality Risk Profile"), 
                                 tags$h6("Please enter patient information and click 'Esitmate Risk' to generate  a risk profile for this complication."),
                                 actionButton("d90btn", "Estimate Risk"),
                                 tags$h3(textOutput("d90")),
                                 tags$br(),
                                 tags$h5(tags$b("About this model:")),
                                 tags$h5(aboutRisk),
                                 tags$h5(riskLevels),
                                 plotOutput("d90RiskPlot")
                                 ),
                        
                        tabPanel("CIin30",
                                 tags$h2("30 Day Chest Infection Risk Profile"),
                                 tags$h6("Please enter patient information and click 'Esitmate Risk' to generate  a risk profile for this complication."),
                                 actionButton("CIbtn", "Estimate Risk"),
                                 tags$h3(textOutput("CI")),
                                 tags$br(),
                                 tags$h5(tags$b("About this model:")),
                                 tags$h5(aboutRisk),
                                 tags$h5(riskLevels),
                                 plotOutput("CIRiskPlot")
                                 ),
                        
                        tabPanel("Infection",
                                 tags$h2("Infection Risk Profile"),
                                 tags$h6("Please enter patient information and click 'Esitmate Risk' to generate  a risk profile for this complication."),
                                 actionButton("Infectionbtn", "Estimate Risk"),
                                 tags$h3(textOutput("Infection")),
                                 tags$br(),
                                 tags$h5(tags$b("About this model:")),
                                 tags$h5(aboutRisk),
                                 tags$h5(riskLevels),
                                 plotOutput("InfectionRiskPlot")
                        )
                        
                        ))
                      
                      #end of tab
                      ),

                      
                      
                      
             tabPanel("About Model",
                      sidebarPanel(
                        tags$h3("About the model:"),
                        tags$h4(AboutNN),
                        tags$br(),
                        tags$img(src="CM.png", height=150, width=240),
                        tags$br(),
                        tags$br(),
                        tags$h5(AboutNNTrain),
                        tags$br(),
                        selectInput(inputId = "AboutModel", label="Please select a complication model to explore:",
                                    c("Transient Ischemic Attack"="aboutTIA", 
                                      "Myocardial infarction"="aboutMI",
                                      "Renal Failure"="aboutRF", 
                                      "Clostridium Difficile "="aboutCDiff", 
                                      "90 Day Mortality"="aboutd90", 
                                      "Chest Infection"="aboutCI",
                                      "Infection"="aboutInfection")),
                        actionButton("aboutModelBtn", "Explore Model")
                      ),
                      
                      
                      mainPanel(
                        tags$h2("Exploring the THR Risk Assessment model:"),
                        tags$h5(exploringModel),
                        tags$h5(aboutCM),
                        tags$h5(tags$b("Accuracy:")),
                        tags$h5(accuracy),
                        tags$h5(tags$b("Specificty:")),
                        tags$h5(specificity),
                        tags$h5(tags$b("Sensitivity:")),
                        tags$h5(sensitivity),
                        plotOutput("aboutPlot")
                      )
                      
                      ),
            
                      
                      
             tabPanel("Model Selection",
                      sidebarPanel(tags$h3(tags$b("Selecting a Machine Learning Model")),
                                   tags$h5("In order to select a model that was suitable for 
                                           investigating the Hospital Episode Statistics dataset and predicting 
                                           risk profiles, Logistic Regression (LR), Random Forest (RF), and Neural Networks (NN) with 
                                           one and two hidden layers were used. "),
                                   tags$h5("For model selection, it was important that for each complication, the model was not only accurate, 
                                           but also had high sensitvity (the ability to identify cases with complications). Because complications are rare,
                                           occuring in roughly 2% of patients, a model would be highly accurate (98%) by predicting a negative outcome for every episode,
                                           however, this model would have 0% selectivity. In order to explore how Neural Networks were selected as the model of choice, select a complication
                                           below to compare the machine learning models."),
                                   radioButtons(inputId = "modelCompare", label="Please select a complication to compare model performance:",
                                          c(
                                                  "Transient Ischemic Attack"="compareTIA",
                                                  "Myocardial infarction"="compareMI",
                                                  "Renal Failure"="compareRF",
                                                  "Clostridium Difficile "="compareCDiff",
                                                  "90 Day Mortality"="compared90",
                                                  "Infection"="compareInfection",
                                                  "Chest Infection"="compareCI"))
                                   ),
                      mainPanel(
                        
                        tabsetPanel(type="tab",
                                  tabPanel("Model Selection",
                                           tags$h3("How Neural Networks Predict Liklihood of Complications"),
                                           tags$h5(tags$b("How to interepret this plot:")),
                                           tags$h5("Since models with high accuracy and high selectivity are desireable, the best models
                                                   for this particilar dataset should appear in the upper-righthand quadrand of these plots. These 
                                                   models all have accuracy over 70% and high selectivity compared with other tested machine learning models."),
                                           plotOutput("modelComparisonPlot"),
                                           tags$h5(tags$b("LR= Logistic Regression, RF= Random Forest, NN1=Neural Network with 1 Hidden Layer (Model Used For THR Risk App),
                                                          NN2= Neural Network with 2 Hidden Layers"))),
                                  
                                  tabPanel("Neural Network Performance",
                                           tags$h5(tags$b("How to interepret this plot:")),
                                           tags$h5("Neural Networks with one hidden layer were chosen as the underlying model for the THR Risk Assessment Application. 
                                                   While these models performed the best when compared to other machine learning models such as Logistic Regression and Random Forest,
                                                   their ability to correctly predict surgical risk varied by complication. Only complications for which risk could be 
                                                   reliably assessed were included in the THR Risk Assessment Application."),
                                           plotOutput("modelPerformancePlot"),
                                           tags$h5(tags$b("CDiff=Clostridium Difficile, CIin30= Chest Infection in 30 Days, CVAin30= Cerebrovascular Accident in 30 Days, 
                                                            died90= Death in 90 Days, Dis18m= Dislocation within 18 months, DVTin90= Deep vein thrombosis within 90 days,
                                                            MIin30= Myocardial infarction within 30 days,PEin90= Pulmonary embolism within 90 days,
                                                          RFin30=Renal Failure within 30 days, TIAin30= Transient ischemic attack within 30 days")))
                       )
                      )
                                           
                                           )
  )
  
  
  
)