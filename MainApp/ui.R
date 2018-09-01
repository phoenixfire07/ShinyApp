library(shiny)
library(shinythemes)
library(caret)

shinyUI(
  navbarPage(
    theme = shinytheme("cosmo"),
    "THR Risk Assessment App",
    tabPanel(
      "Start",
      # ---------------------------------start page begins----------------------------------------
      fluidPage(
        tags$h1(tags$b(
          div(
            style = "text-align:center;",
            "Welcome to the Total Hip Replacement Risk Assessment Application"
          )
        ),
        tags$br()),
        div(
          style =
            "width:280px;
          height:500px;
          background-image: url(THR.jpg);
          background-size:cover;
          background-repeat: no-repeat;
          margin: auto;
          "
        ),
        tags$h3(tags$b("Background information:")),
        tags$h4(textOutput("aboutTHRbackground")),
        tags$br()
        ),
      fluidPage(sidebarPanel(
        radioButtons(
          inputId = "yearlyStats",
          label = "Please select a complication to view number of yearly episodes as
          recorded by the HES from 2005 to 2014:",
          c(
            "Total THR Surgeries" = "yearlyTotal",
            "Transient Ischemic Attack" = "yearlyTIA",
            "Myocardial infarction" = "yearlyMI",
            "Renal Failure" = "yearlyRF",
            "Clostridium Difficile " = "yearlyCDiff",
            "90 Day Mortality" = "yearlyd90",
            "Chest Infection" = "yearlyCI",
            "Infection" = "yearlyInfection"
          )
        )
      ),
      mainPanel(plotOutput("yearlyStatsPlot"))),
      tags$br(),
      fluidPage(
        tags$h3(tags$b("Objective of this tool:")),
        tags$h4(textOutput("appObjective")),
        tags$br(),
        tags$h3(tags$b("How to use the THR Risk Assessment App:")),
        column(6,
               tags$h4(tags$b("Risk Assessment:")),
               tags$h4(textOutput("riskAssessment"))),
        column(
          6,
          tags$h4(tags$b("Explore Model:")),
          tags$h4(textOutput("exploreModel")),
          tags$br(),
          tags$br(),
          tags$br()
        ),
        fluidPage(
          column(
            6,
            div(
              style =
                "display: block;
              margin-left: auto;
              margin-right: auto;
              width: 50%;",
              tags$img(
                src = "ucl.png",
                height = 90,
                width = 270
              )
            ),
            tags$br(),
            tags$br()
        ),
        column(
          6,
          div(
            style =
              "display: block;
            margin-left: auto;
            margin-right: auto;
            width: 50%;",
            tags$img(
              src = "nhs.png",
              height = 90,
              width = 210
            )
          ),
          tags$br(),
          tags$br()
      )))),
    # ---------------------------------start page ends----------------------------------------
    tabPanel(
      "Risk Assessment",
      # ---------------------------------risk assessment page begins----------------------------------------
      sidebarPanel(
        tags$h3("Patient information input panel:"),
        sliderInput(
          inputId = "Age",
          label = "Select Age",
          value = 65,
          min = 30,
          max = 110
        ),
        radioButtons(
          inputId = "Sex",
          label = "Gender:",
          c("Male" = .1111111111,
            "Female" = .222222222)
        ),
        selectInput(inputId = "CIHD", label =
                      "Does the patient have a history with Chronic Ischemic Heart Disease (CIHD)?",
                    c("No" = 0,
                      "Yes" = 1)),
        selectInput(inputId = "Hyperthyroidism", label =
                      "Does the patient have a history with Hyperthyroidism (HyperT)?",
                    c("No" = 0,
                      "Yes" = 1)),
        selectInput(inputId = "Hypothyroidism", label =
                      "Does the patient have a history with Hypothyroidism (HypoT)?",
                    c("No" = 0,
                      "Yes" = 1)),
        selectInput(inputId = "HT", label = "Does the patient have a history with Hypertension (HT)?",
                    c("No" = 0,
                      "Yes" = 1)),
        selectInput(inputId = "AF", label = "Does the patient have a history with Atrial fibrillation (AF)?",
                    c("No" = 0,
                      "Yes" = 1)),
        selectInput(inputId = "IDDM", label =
                      "Does the patient have a history with Insulin-Dependent Diabetes Mellitus (IDDM)?",
                    c("No" = 0,
                      "Yes" = 1)),
        selectInput(inputId = "NIDDM", label =
                      "Does the patient have a history with Non-Insulin-Dependent Diabetes Mellitus (NIDDM)?",
                    c("No" = 0,
                      "Yes" = 1)),
        selectInput(inputId = "HCD", label =
                      "Does the patient have a history with Circulatory Disease (HCD)?",
                    c("No" = 0,
                      "Yes" = 1)),
        selectInput(inputId = "COPD", label =
                      "Does the patient have a history with Chronic Obstructive Pulmonary Disease (COPD)?",
                    c("No" = 0,
                      "Yes" = 1)),
        selectInput(inputId = "Osteoporosis", label =
                      "Does the patient have a history with Osteoporosis (Ost)?",
                    c("No" = 0,
                      "Yes" = 1)),
        selectInput(inputId = "HC", label = "Does the patient have a history with Hypercholestrolemia (HC)?",
                    c("No" = 0,
                      "Yes" = 1)),
        selectInput(inputId = "HCVA", label =
                      "Does the patient have a history with Hemorrhagic Cerebrovascular Accident (HCVA)?",
                    c("No" = 0,
                      "Yes" = 1)),
        selectInput(inputId = "Dementia", label =
                      "Does the patient have a history with Dementia (D)?",
                    c("No" = 0,
                      "Yes" = 1)),
        selectInput(inputId = "Alz", label =
                      "Does the patient have a history with Alzheimers (Alz)?",
                    c("No" = 0,
                      "Yes" = 1)),
        selectInput(inputId = "DU", label = "Does the patient have a history with Duodenal Ulcers (DU)?",
                    c("No" = 0,
                      "Yes" = 1))
      ),
      mainPanel(
        tabsetPanel(
          type = "tab",
          tabPanel(
            "RFin30",
            tags$h2("Estimated 30 Day Renal Failure Risk Profile"),
            tags$h6(
              "Please enter patient information and click 'Esitmate Risk' to generate  a risk profile for this complication."
            ),
            actionButton("RFbtn", "Estimate Risk"),
            tags$h3(textOutput("RF")),
            tags$br(),
            plotOutput("RFRiskPlot"),
            tags$h6(
              "The neural networks for this tool were generated using ",
              tags$a(href = "https://cran.r-project.org/web/packages/neuralnet/neuralnet.pdf", "neuralnet")
            ),
            tags$br(),
            tags$h4(tags$b("Estimating variable importance:")),
            plotOutput("RFVarImpPlot")),
          tabPanel(
            "MIin30",
            tags$h2("30 Day Myocardial Infarction Risk Profile"),
            tags$h6(
              "Please enter patient information and click 'Esitmate Risk' to generate  a risk profile for this complication."
            ),
            actionButton("MIbtn", "Estimate Risk"),
            tags$h3(textOutput("MI")),
            tags$br(),
            plotOutput("MIRiskPlot"),
            tags$h6(
              "The neural networks for this tool were generated using ",
              tags$a(href = "https://cran.r-project.org/web/packages/neuralnet/neuralnet.pdf", "neuralnet")
            ),
            tags$br(),
           plotOutput("MIVarImpPlot")),
          tabPanel(
            "TIAin30",
            tags$h2("30 Day Transient Ischemic Attack Risk Profile"),
            tags$h6(
              "Please enter patient information and click 'Esitmate Risk' to generate  a risk profile for this complication."
            ),
            actionButton("TIAbtn", "Estimate Risk"),
            tags$h3(textOutput("TIA")),
            tags$br(),
            plotOutput("TIARiskPlot"),
            tags$h6(
              "The neural networks for this tool were generated using ",
              tags$a(href = "https://cran.r-project.org/web/packages/neuralnet/neuralnet.pdf", "neuralnet")
            ),
            tags$br(),
            tags$h4(tags$b("Estimating variable importance:")),
            plotOutput("TIAVarImpPlot")),
          tabPanel(
            "CDiff",
            tags$h2("Clostridium Difficile Risk Profile"),
            tags$h6(
              "Please enter patient information and click 'Esitmate Risk' to generate  a risk profile for this complication."
            ),
            actionButton("CDiffbtn", "Estimate Risk"),
            tags$h3(textOutput("CDiff")),
            tags$br(),
            plotOutput("CDiffRiskPlot"),
            tags$h6(
              "The neural networks for this tool were generated using ",
              tags$a(href = "https://cran.r-project.org/web/packages/neuralnet/neuralnet.pdf", "neuralnet")
            ),
            tags$br(),
            tags$h4(tags$b("Estimating variable importance:")),
            plotOutput("CDiffVarImpPlot")),
          tabPanel(
            "Death90",
            tags$h2("90 Day Mortality Risk Profile"),
            tags$h6(
              "Please enter patient information and click 'Esitmate Risk' to generate  a risk profile for this complication."
            ),
            actionButton("d90btn", "Estimate Risk"),
            tags$h3(textOutput("d90")),
            tags$br(),
            plotOutput("d90RiskPlot"),
            tags$h6(
              "The neural networks for this tool were generated using ",
              tags$a(href = "https://cran.r-project.org/web/packages/neuralnet/neuralnet.pdf", "neuralnet")
            ),
            tags$br(),
            tags$h4(tags$b("Estimating variable importance:")),
            plotOutput("d90VarImpPlot")),
          tabPanel(
            "CIin30",
            tags$h2("30 Day Chest Infection Risk Profile"),
            tags$h6(
              "Please enter patient information and click 'Esitmate Risk' to generate  a risk profile for this complication."
            ),
            actionButton("CIbtn", "Estimate Risk"),
            tags$h3(textOutput("CI")),
            tags$br(),
            plotOutput("CIRiskPlot"),
            tags$h6(
              "The neural networks for this tool were generated using ",
              tags$a(href = "https://cran.r-project.org/web/packages/neuralnet/neuralnet.pdf", "neuralnet")
            ),
           
            tags$br(),
            tags$h4(tags$b("Estimating variable importance:")),
            plotOutput("CIVarImpPlot")),
          tabPanel(
            "Infection",
            tags$h2("Infection Risk Profile"),
            tags$h6(
              "Please enter patient information and click 'Esitmate Risk' to generate  a risk profile for this complication."
            ),
            actionButton("Infectionbtn", "Estimate Risk"),
            tags$h3(textOutput("Infection")),
            tags$br(),
            plotOutput("InfectionRiskPlot"),
            tags$h6(
              "The neural networks for this tool were generated using ",
              tags$a(href = "https://cran.r-project.org/web/packages/neuralnet/neuralnet.pdf", "neuralnet")
            ),
            tags$br(),
            tags$h4(tags$b("Estimating variable importance:")),
            plotOutput("InfectionVarImpPlot"))
        )
      ),
      tags$h6(
        "Variable importance was generated using the olden funtion in the",
        tags$a(href = "https://cran.r-project.org/web/packages/NeuralNetTools/NeuralNetTools.pdf", "NeuralNetTools"),
        " package. This algorithm was proposed in ",
        tags$a(href = "http://depts.washington.edu/oldenlab/wordpress/wp-content/uploads/2013/03/EcologicalModelling_2004.pdf",
               "Olden et al. 2004")
      ),
      tags$h5(tags$b(textOutput("VarImpKey"))),
      tags$h4(tags$b("About Variable Importance:")),
      tags$h5(textOutput("VarImpHow")),
      
      tags$h4(tags$b("About this model:")),
      tags$h5(textOutput("aboutRisk")),
      tags$h5(
        textOutput("riskNote"),
        tags$a(href = "https://www.sas.com/en_us/home.html", "SAS"),
        " and by Gordon S. Linoff, co-author of ",
        tags$a(href = "http://www.data-miners.com/bookstore.htm", "Mastering Data Mining."),
        "The method can be found ",
        tags$a(href = "http://support.sas.com/kb/22/601.html", "here"),
        " and ",
        tags$a(href = "http://blog.data-miners.com/2009/09/adjusting-for-oversampling.html", "here.")
      ),
      tags$br()
    ),
    # ---------------------------------risk assessment page ends----------------------------------------
    tabPanel(
      "About Model",
      # ---------------------------------about model page begins----------------------------------------
      sidebarPanel(
        tags$h3("About the model:"),
        tags$h4(textOutput("aboutNN")),
        tags$br(),
        tags$img(
          src = "CM.png",
          height = 150,
          width = 240),
        tags$br(),
        tags$br(),
        tags$h5(textOutput("aboutNNTrain")),
        tags$br(),
        selectInput(
          inputId = "AboutModel",
          label = "Please select a complication model to explore:",
          c(
            "Transient Ischemic Attack" = "aboutTIA",
            "Myocardial infarction" = "aboutMI",
            "Renal Failure" = "aboutRF",
            "Clostridium Difficile " = "aboutCDiff",
            "90 Day Mortality" = "aboutd90",
            "Chest Infection" = "aboutCI",
            "Infection" = "aboutInfection"
          )),
        actionButton("aboutModelBtn", "Explore Model")),
      mainPanel(
        tags$h2("Exploring the THR Risk Assessment model:"),
        tags$h5(textOutput("exploringModel")),
        tags$h5(textOutput("aboutCM")),
        tags$h5(tags$b("Accuracy:")),
        tags$h5(textOutput("accuracy")),
        tags$h5(tags$b("Specificty:")),
        tags$h5(textOutput("specificity")),
        tags$h5(tags$b("Sensitivity:")),
        tags$h5(textOutput("sensitivity")),
        plotOutput("aboutPlot")
      )),
    # ---------------------------------about model page ends----------------------------------------
    tabPanel(
      # ---------------------------------model selection page begins----------------------------------------
      "Model Selection",
      sidebarPanel(
        tags$h3(tags$b("Selecting a Machine Learning Model")),
        tags$h5(textOutput("selection")),
        radioButtons(
          inputId = "modelCompare",
          label = "Please select a complication to compare model performance:",
          c(
            "Transient Ischemic Attack" = "compareTIA",
            "Myocardial infarction" =
              "compareMI",
            "Renal Failure" = "compareRF",
            "Clostridium Difficile " =
              "compareCDiff",
            "90 Day Mortality" = "compared90",
            "Infection" = "compareInfection",
            "Chest Infection" = "compareCI"
          ))),
      mainPanel(tabsetPanel(
        type = "tab",
        tabPanel(
          "Model Selection",
          tags$h3("How Neural Networks Predict Liklihood of Complications"),
          tags$h5(tags$b("How to interepret this plot:")),
          tags$h5(textOutput("req")),
          plotOutput("modelComparisonPlot"),
          tags$h5(
            tags$b(
              "LR= Logistic Regression, RF= Random Forest, NN1=Neural Network with 1 Hidden Layer (Model Used For THR Risk App),
              NN2= Neural Network with 2 Hidden Layers"
            ))),
        tabPanel(
          "Neural Network Performance",
          tags$h5(tags$b("How to interepret this plot:")),
          tags$h5(textOutput("NN1")
          ),
          plotOutput("modelPerformancePlot"),
          tags$h5(tags$b(textOutput("plotkey"))
          ))
        ))))
  # ---------------------------------model selection page ends----------------------------------------
)