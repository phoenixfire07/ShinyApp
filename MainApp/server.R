library(shiny)
library(neuralnet)
library(caret)
library(ggplot2)
library(NeuralNetTools)

shinyServer(function(input, output) {
# importing neural network models 
  
  NNRF<- readRDS("NN/RFin30.rda")
  NNMI<- readRDS("NN/MIin30.rda")
  NNd90<- readRDS("NN/died90.rda")
  NNCDiff<- readRDS("NN/CDiff.rda")
  NNCI<- readRDS("NN/CIin30.rda")
  NNTIA<- readRDS("NN/TIAin30.rda")
  NNInfection<- readRDS("NN/Infection.rda")
  
  #importing plot csv files
  
  aboutPlotCDiff <- read.csv("csvs/CDiff.csv")
  aboutPlotMI <- read.csv("csvs/MIin30.csv")
  aboutPlotCI <- read.csv("csvs/CIin30.csv")
  aboutPlotRF <- read.csv("csvs/RFin30.csv")
  aboutPlotTIA <- read.csv("csvs/TIAin30.csv")
  aboutPlotd90 <- read.csv("csvs/died90.csv")
  aboutPlotInfection <- read.csv("csvs/Infection.csv")
  
  #importing yearly surgery stats file
  years<- read.csv("csvs/yearlyStats.csv")
  
  #importing NN model performance file
  
  modelPerformance <- read.csv("csvs/NN_Sen_Acc.csv")
  
  #importing model comparison files
  
  modelCompareCDiff <- read.csv("csvs/CDiffModels.csv")
  modelCompareCI <- read.csv("csvs/CIModels.csv")
  modelCompareRF <- read.csv("csvs/RFModels.csv")
  modelCompareTIA <- read.csv("csvs/TIAModels.csv")
  modelCompareMI <- read.csv("csvs/MIin30Models.csv")
  modelCompared90 <- read.csv("csvs/died90Models.csv")
  modelCompareInfection <- read.csv("csvs/InfectionModels.csv")
 
#NN Plots -------------------------------------------------
  
  output$RFRiskPlot <- renderPlot({
    plot(NNRF, rep="best")
  })
  
   output$MIRiskPlot <- renderPlot({
    plot(NNMI, rep="best")
  })
  
  output$d90RiskPlot <- renderPlot({
    plot(NNd90, rep="best")
  })
  
  output$CDiffRiskPlot <- renderPlot({
    plot(NNCDiff, rep="best")
  })
  
  output$CIRiskPlot <- renderPlot({
    plot(NNCI, rep="best")
  })
  
  output$TIARiskPlot <- renderPlot({
    plot(NNTIA, rep="best")
  })
  
  output$InfectionRiskPlot <- renderPlot({
    plot(NNInfection, rep="best")
  })


#End of NN Plots -------------------------------------------------

# Variable Importance Plots--------------------------------------------

output$RFVarImpPlot<- renderPlot({
  olden(NNRF,x_lab=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"))
})

output$MIVarImpPlot<- renderPlot({
  olden(NNMI, x_lab=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"))
})

output$TIAVarImpPlot<- renderPlot({
  olden(NNTIA, x_lab=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"))
})

output$CDiffVarImpPlot<- renderPlot({
  olden(NNCDiff, x_lab=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"))
})

output$d90VarImpPlot<- renderPlot({
  olden(NNd90, x_lab=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"))
})

output$CIVarImpPlot<- renderPlot({
  olden(NNCI, x_lab=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"))
})

output$InfectionVarImpPlot<- renderPlot({
  olden(NNInfection, x_lab=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"))
})

# End of Variable Importance Plots--------------------------------------------


# Renal Failure Risk Estimate -----------------------------------------------------------
  
  
  textRF<- eventReactive(input$RFbtn,{
    
    threshold<- input$Threshold/100
    
    age<- ((input$Age-1)/108)
    
    response<- data.frame("Age"=as.numeric(age), "Sex"=as.numeric(input$Sex) , "CIHD"=as.numeric(input$CIHD), "HyperT"=as.numeric(input$Hyperthyroidism) , "HypoT"=as.numeric(input$Hypothyroidism) , "IDDM"=as.numeric(input$IDDM) , "NIDDM"=as.numeric(input$NIDDM) , "HCD"=as.numeric(input$HCD), "COPD"=as.numeric(input$COPD) , "D"=as.numeric(input$Dementia), "Alz"=as.numeric(input$Alz) ,
                          "DU"=as.numeric(input$DU) , "Ost"=as.numeric(input$Osteoporosis) , "HC"=as.numeric(input$HC) , "HCVA"=as.numeric(input$HCVA) , "HT"=as.numeric(input$HT) , "AF"=as.numeric(input$AF))

    pred_RF<-compute((NNRF),response)

    pred_RF<-unlist(pred_RF)

    pred_RF<-pred_RF["net.result"]

    pred_RF<- unname(pred_RF)
    
    RFcommon<-99.321
    RFrare<-0.679
    startingPrediction<-pred_RF
    rareChange<- 50/RFrare
    commonChange<-50/RFcommon
    rareAdjusted<-startingPrediction/rareChange
    commonAdjusted<-(1-startingPrediction)/commonChange
    finalprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)), digits = 3)
    
    moderatePred<-0.5
    rareChange<- 50/RFrare
    commonChange<-50/RFcommon
    rareAdjusted<-moderatePred/rareChange
    commonAdjusted<-(1-moderatePred)/commonChange
    moderateprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)), digits = 3)
    
    highPred<-.75
    rareChange<- 50/RFrare
    commonChange<-50/RFcommon
    rareAdjusted<-highPred/rareChange
    commonAdjusted<-(1-highPred)/commonChange
    highprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)),digits = 3)
    
    
    if(finalprediction>highprediction){
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of Renal Failure within 30 days post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk above ",highprediction,"% is considered to be HIGH risk compared to the average population.", sep="")
      output
    }
    else if(finalprediction>moderateprediction && finalprediction<highprediction)  {
      
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of Renal Failure within 30 days post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk above ",moderateprediction, "% and below ",highprediction,"% is considered to be MODERATE risk compared to the average population.", sep="")
      
      output
    }
    
    else{
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of Renal Failure within 30 days post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk below ",moderateprediction, "% is considered to be LOW risk compared to the average population.", sep="")
      
      output
    }
   
  })

  output$RF <- renderText({
    textRF()
  })
  
 
# End of Renal Failure Risk Estimate -----------------------------------------------------------
  
# MI Risk Estimate -----------------------------------------------------------------------------

  textMI<- eventReactive(input$MIbtn,{
    
    threshold<- input$Threshold/100
    
    age<- ((input$Age-1)/108)
    
    response<- data.frame("Age"=as.numeric(age), "Sex"=as.numeric(input$Sex) , "CIHD"=as.numeric(input$CIHD), "HyperT"=as.numeric(input$Hyperthyroidism) , "HypoT"=as.numeric(input$Hypothyroidism) , "IDDM"=as.numeric(input$IDDM) , "NIDDM"=as.numeric(input$NIDDM) , "HCD"=as.numeric(input$HCD), "COPD"=as.numeric(input$COPD) , "D"=as.numeric(input$Dementia), "Alz"=as.numeric(input$Alz) ,
                          "DU"=as.numeric(input$DU) , "Ost"=as.numeric(input$Osteoporosis) , "HC"=as.numeric(input$HC) , "HCVA"=as.numeric(input$HCVA) , "HT"=as.numeric(input$HT) , "AF"=as.numeric(input$AF))
    
    pred_MI<-compute((NNMI),response)
    
    pred_MI<-unlist(pred_MI)
    
    pred_MI<-pred_MI["net.result"]
    
    pred_MI<- unname(pred_MI)
    
    MIcommon<-99.677
    MIrare<-0.323
    startingPrediction<-pred_MI
    rareChange<- 50/MIrare
    commonChange<-50/MIcommon
    rareAdjusted<-startingPrediction/rareChange
    commonAdjusted<-(1-startingPrediction)/commonChange
    finalprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)), digits = 3)
    
    moderatePred<-0.5
    rareChange<- 50/MIrare
    commonChange<-50/MIcommon
    rareAdjusted<-moderatePred/rareChange
    commonAdjusted<-(1-moderatePred)/commonChange
    moderateprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)), digits = 3)
    
    highPred<-.75
    rareChange<- 50/MIrare
    commonChange<-50/MIcommon
    rareAdjusted<-highPred/rareChange
    commonAdjusted<-(1-highPred)/commonChange
    highprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)),digits = 3)
    
    
    if(finalprediction>highprediction){
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of Myocardial Infarction within 30 days post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk above ",highprediction,"% is considered to be HIGH risk compared to the average population.", sep="")
      output
    }
    else if(finalprediction>moderateprediction && finalprediction<highprediction)  {
      
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of Myocardial Infarction within 30 days post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk above ",moderateprediction, "% and below ",highprediction,"% is considered to be MODERATE risk compared to the average population.", sep="")
      
      output
    }
    
    else{
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of Myocardial Infarction within 30 days post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk below ",moderateprediction, "% is considered to be LOW risk compared to the average population.", sep="")
      
      output
    }
    
    })
  
  output$MI <- renderText({
    textMI()
  })
  
#End of MI Risk Estimate -----------------------------------------------------------------------------

#TIA Risk Estimate -----------------------------------------------------------------------------
  
  textTIA<- eventReactive(input$TIAbtn,{
    
   age<- ((input$Age-1)/108)
    
    response<- data.frame("Age"=as.numeric(age), "Sex"=as.numeric(input$Sex) , "CIHD"=as.numeric(input$CIHD), "HyperT"=as.numeric(input$Hyperthyroidism) , "HypoT"=as.numeric(input$Hypothyroidism) , "IDDM"=as.numeric(input$IDDM) , "NIDDM"=as.numeric(input$NIDDM) , "HCD"=as.numeric(input$HCD), "COPD"=as.numeric(input$COPD) , "D"=as.numeric(input$Dementia), "Alz"=as.numeric(input$Alz) ,
                          "DU"=as.numeric(input$DU) , "Ost"=as.numeric(input$Osteoporosis) , "HC"=as.numeric(input$HC) , "HCVA"=as.numeric(input$HCVA) , "HT"=as.numeric(input$HT) , "AF"=as.numeric(input$AF))
    
    pred_TIA<-compute((NNTIA),response)
    
    pred_TIA<-unlist(pred_TIA)
    
    pred_TIA<-pred_TIA["net.result"]
    
    pred_TIA<- unname(pred_TIA)
    
    TIAcommon<-99.913
    TIArare<-0.087
    startingPrediction<-pred_TIA
    rareChange<- 50/TIArare
    commonChange<-50/TIAcommon
    rareAdjusted<-startingPrediction/rareChange
    commonAdjusted<-(1-startingPrediction)/commonChange
    finalprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)), digits = 3)
    
    moderatePred<-0.5
    rareChange<- 50/TIArare
    commonChange<-50/TIAcommon
    rareAdjusted<-moderatePred/rareChange
    commonAdjusted<-(1-moderatePred)/commonChange
    moderateprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)), digits = 3)
    
    highPred<-.75
    rareChange<- 50/TIArare
    commonChange<-50/TIAcommon
    rareAdjusted<-highPred/rareChange
    commonAdjusted<-(1-highPred)/commonChange
    highprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)),digits = 3)
    
    
    if(finalprediction>highprediction){
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of Transient Ischemic Attack within 30 days post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk above ",highprediction,"% is considered to be HIGH risk compared to the average population.", sep="")
      output
    }
    else if(finalprediction>moderateprediction && finalprediction<highprediction)  {
      
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of Transient Ischemic Attack within 30 days post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk above ",moderateprediction, "% and below ",highprediction,"% is considered to be MODERATE risk compared to the average population.", sep="")
      
      output
    }
    
    else{
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of Transient Ischemic Attack within 30 days post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk below ",moderateprediction, "% is considered to be LOW risk compared to the average population.", sep="")
      
      output
    }
    
    })
  
  output$TIA <- renderText({
    textTIA()
  })
  
#End of TIA Risk Estimate -----------------------------------------------------------------------------

#CDiff Risk Estimate -----------------------------------------------------------------------------
  
  textCDiff<- eventReactive(input$CDiffbtn,{
    
    age<- ((input$Age-1)/108)
    
    response<- data.frame("Age"=as.numeric(age), "Sex"=as.numeric(input$Sex) , "CIHD"=as.numeric(input$CIHD), "HyperT"=as.numeric(input$Hyperthyroidism) , "HypoT"=as.numeric(input$Hypothyroidism) , "IDDM"=as.numeric(input$IDDM) , "NIDDM"=as.numeric(input$NIDDM) , "HCD"=as.numeric(input$HCD), "COPD"=as.numeric(input$COPD) , "D"=as.numeric(input$Dementia), "Alz"=as.numeric(input$Alz) ,
                          "DU"=as.numeric(input$DU) , "Ost"=as.numeric(input$Osteoporosis) , "HC"=as.numeric(input$HC) , "HCVA"=as.numeric(input$HCVA) , "HT"=as.numeric(input$HT) , "AF"=as.numeric(input$AF))
    
    pred_CDiff<-compute((NNCDiff),response)
    
    pred_CDiff<-unlist(pred_CDiff)
    
    pred_CDiff<-pred_CDiff["net.result"]
    
    pred_CDiff<- unname(pred_CDiff)
    
    
    CDiffcommon<-99.928
    CDiffrare<-0.072
    startingPrediction<-pred_CDiff
    rareChange<- 50/CDiffrare
    commonChange<-50/CDiffcommon
    rareAdjusted<-startingPrediction/rareChange
    commonAdjusted<-(1-startingPrediction)/commonChange
    finalprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)), digits = 3)
    
    moderatePred<-0.5
    rareChange<- 50/CDiffrare
    commonChange<-50/CDiffcommon
    rareAdjusted<-moderatePred/rareChange
    commonAdjusted<-(1-moderatePred)/commonChange
    moderateprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)), digits = 3)
    
    highPred<-.75
    rareChange<- 50/CDiffrare
    commonChange<-50/CDiffcommon
    rareAdjusted<-highPred/rareChange
    commonAdjusted<-(1-highPred)/commonChange
    highprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)),digits = 3)
    
    
    if(finalprediction>highprediction){
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of Clostridium Difficile post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk above ",highprediction,"% is considered to be HIGH risk compared to the average population.", sep="")
      output
    }
    else if(finalprediction>moderateprediction && finalprediction<highprediction)  {
      
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of Clostridium Difficile post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk above ",moderateprediction, "% and below ",highprediction,"% is considered to be MODERATE risk compared to the average population.", sep="")
      
      output
    }
    
    else{
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of Clostridium Difficile post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk below ",moderateprediction, "% is considered to be LOW risk compared to the average population.", sep="")
    
      output
    }
    
    })
  
  output$CDiff <- renderText({
    textCDiff()
  })
  
#End of CDiff Risk Estimate -----------------------------------------------------------------------------  

  

#Died90 Risk Estimate -----------------------------------------------------------------------------
  
  textd90<- eventReactive(input$d90btn,{
    
    age<- ((input$Age-1)/108)
    
    response<- data.frame("Age"=as.numeric(age), "Sex"=as.numeric(input$Sex) , "CIHD"=as.numeric(input$CIHD), "HyperT"=as.numeric(input$Hyperthyroidism) , "HypoT"=as.numeric(input$Hypothyroidism) , "IDDM"=as.numeric(input$IDDM) , "NIDDM"=as.numeric(input$NIDDM) , "HCD"=as.numeric(input$HCD), "COPD"=as.numeric(input$COPD) , "D"=as.numeric(input$Dementia), "Alz"=as.numeric(input$Alz) ,
                          "DU"=as.numeric(input$DU) , "Ost"=as.numeric(input$Osteoporosis) , "HC"=as.numeric(input$HC) , "HCVA"=as.numeric(input$HCVA) , "HT"=as.numeric(input$HT) , "AF"=as.numeric(input$AF))
    
    pred_d90<-compute((NNd90),response)
    
    pred_d90<-unlist(pred_d90)
    
    pred_d90<-pred_d90["net.result"]
    
    pred_d90<- unname(pred_d90)
    
    died90common<-99.713
    died90rare<-0.286
    startingPrediction<-pred_d90
    rareChange<- 50/died90rare
    commonChange<-50/died90common
    rareAdjusted<-startingPrediction/rareChange
    commonAdjusted<-(1-startingPrediction)/commonChange
    finalprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)), digits = 3)
    
    moderatePred<-0.5
    rareChange<- 50/died90rare
    commonChange<-50/died90common
    rareAdjusted<-moderatePred/rareChange
    commonAdjusted<-(1-moderatePred)/commonChange
    moderateprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)), digits = 3)
    
    highPred<-.75
    rareChange<- 50/died90rare
    commonChange<-50/died90common
    rareAdjusted<-highPred/rareChange
    commonAdjusted<-(1-highPred)/commonChange
    highprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)),digits = 3)
    
   
    if(finalprediction>highprediction){
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of death 90 days post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk above ",highprediction,"% is considered to be HIGH risk compared to the average population.", sep="")
      output
    }
    else if(finalprediction>moderateprediction && finalprediction<highprediction)  {

      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of death 90 days post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk above ",moderateprediction, "% and below ",highprediction,"% is considered to be MODERATE risk compared to the average population.", sep="")
      
      output
    }

    else{
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of death 90 days post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk below ",moderateprediction, "% is considered to be LOW risk compared to the average population.", sep="")
      
      output
    }

    })
  
  output$d90 <- renderText({
    textd90()
  })
  
#End of Died90 Risk Estimate -----------------------------------------------------------------------------

#CI Risk Estimate -----------------------------------------------------------------------------
  
  textCI<- eventReactive(input$CIbtn,{
    
    threshold<- input$Threshold/100
    
    age<- ((input$Age-1)/108)
    
    response<- data.frame("Age"=as.numeric(age), "Sex"=as.numeric(input$Sex) , "CIHD"=as.numeric(input$CIHD), "HyperT"=as.numeric(input$Hyperthyroidism) , "HypoT"=as.numeric(input$Hypothyroidism) , "IDDM"=as.numeric(input$IDDM) , "NIDDM"=as.numeric(input$NIDDM) , "HCD"=as.numeric(input$HCD), "COPD"=as.numeric(input$COPD) , "D"=as.numeric(input$Dementia), "Alz"=as.numeric(input$Alz) ,
                          "DU"=as.numeric(input$DU) , "Ost"=as.numeric(input$Osteoporosis) , "HC"=as.numeric(input$HC) , "HCVA"=as.numeric(input$HCVA) , "HT"=as.numeric(input$HT) , "AF"=as.numeric(input$AF))
    
    pred_CI<-compute((NNCI),response)
    
    pred_CI<-unlist(pred_CI)
    
    pred_CI<-pred_CI["net.result"]
    
    pred_CI<- unname(pred_CI)
    
    CIcommon<-99.235
    CIrare<-0.765
    startingPrediction<-pred_CI
    rareChange<- 50/CIrare
    commonChange<-50/CIcommon
    rareAdjusted<-startingPrediction/rareChange
    commonAdjusted<-(1-startingPrediction)/commonChange
    finalprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)), digits = 3)
    
    moderatePred<-0.5
    rareChange<- 50/CIrare
    commonChange<-50/CIcommon
    rareAdjusted<-moderatePred/rareChange
    commonAdjusted<-(1-moderatePred)/commonChange
    moderateprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)), digits = 3)
    
    highPred<-.75
    rareChange<- 50/CIrare
    commonChange<-50/CIcommon
    rareAdjusted<-highPred/rareChange
    commonAdjusted<-(1-highPred)/commonChange
    highprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)),digits = 3)
    
    
    if(finalprediction>highprediction){
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of Chest Infection within 30 days post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk above ",highprediction,"% is considered to be HIGH risk compared to the average population.", sep="")
      output
    }
    else if(finalprediction>moderateprediction && finalprediction<highprediction)  {
      
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of Chest Infection within 30 days post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk above ",moderateprediction, "% and below ",highprediction,"% is considered to be MODERATE risk compared to the average population.", sep="")
      
      output
    }
    
    else{
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of Chest Infection within 30 days post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk below ",moderateprediction, "% is considered to be LOW risk compared to the average population.", sep="")
      
      output
    }
    
    })
  
  output$CI<- renderText({
    textCI()
  })
  
  # end of CI risk estimate---------------------------------------------------------------------
  
  
  #infectionRisk Estimate -----------------------------------------------------------------------------
  
  textInfection<- eventReactive(input$Infectionbtn,{
    
    threshold<- input$Threshold/100
    
    age<- ((input$Age-1)/108)
    
    response<- data.frame("Age"=as.numeric(age), "Sex"=as.numeric(input$Sex) , "CIHD"=as.numeric(input$CIHD), "HyperT"=as.numeric(input$Hyperthyroidism) , "HypoT"=as.numeric(input$Hypothyroidism) , "IDDM"=as.numeric(input$IDDM) , "NIDDM"=as.numeric(input$NIDDM) , "HCD"=as.numeric(input$HCD), "COPD"=as.numeric(input$COPD) , "D"=as.numeric(input$Dementia), "Alz"=as.numeric(input$Alz) ,
                          "DU"=as.numeric(input$DU) , "Ost"=as.numeric(input$Osteoporosis) , "HC"=as.numeric(input$HC) , "HCVA"=as.numeric(input$HCVA) , "HT"=as.numeric(input$HT) , "AF"=as.numeric(input$AF))
    
    pred_infection<-compute((NNInfection),response)
    
    pred_infection<-unlist(pred_infection)
    
    pred_infection<-pred_infection["net.result"]
    
    pred_infection<- unname(pred_infection)
    
    infectioncommon<-99.026
    infectionrare<-0.974
    startingPrediction<-pred_infection
    rareChange<- 50/infectionrare
    commonChange<-50/infectioncommon
    rareAdjusted<-startingPrediction/rareChange
    commonAdjusted<-(1-startingPrediction)/commonChange
    finalprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)), digits = 3)
    
    moderatePred<-0.5
    rareChange<- 50/infectionrare
    commonChange<-50/infectioncommon
    rareAdjusted<-moderatePred/rareChange
    commonAdjusted<-(1-moderatePred)/commonChange
    moderateprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)), digits = 3)
    
    highPred<-.75
    rareChange<- 50/infectionrare
    commonChange<-50/infectioncommon
    rareAdjusted<-highPred/rareChange
    commonAdjusted<-(1-highPred)/commonChange
    highprediction<-round(100*(rareAdjusted/(rareAdjusted+commonAdjusted)),digits = 3)
    
    
    if(finalprediction>highprediction){
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of infection post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk above ",highprediction,"% is considered to be HIGH risk compared to the average population.", sep="")
      output
    }
    else if(finalprediction>moderateprediction && finalprediction<highprediction)  {
      
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of infection post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk above ",moderateprediction, "% and below ",highprediction,"% is considered to be MODERATE risk compared to the average population.", sep="")
      
      output
    }
    
    else{
      output <-paste("Based on the information provided, the patient is estimated to have a ", finalprediction, 
                     "% probability of infection post surgery. Since the average risk for this complication is ",moderateprediction, 
                     "%, any risk below ",moderateprediction, "% is considered to be LOW risk compared to the average population.", sep="")
      
      output
    }
    
    
    })
  
  output$Infection <- renderText({
    textInfection()
  })
  
  #End of infection Risk Estimate -----------------------------------------------------------------------------
  
  # generating about barplots for 'About Model' section --------------------------
  
  aboutModelPlot<- eventReactive(input$aboutModelBtn,{
    
    if(input$AboutModel=="aboutCI"){
      
      CIplot<- barplot(c(aboutPlotCI$Accuracy,aboutPlotCI$Specificity,aboutPlotCI$Sensitivity),
              names.arg = c("Accuracy","Specificity","Sensitivity"), 
              main="About 30 Day Chest Infection Risk Model",
              horiz = F, 
              ylim=c(0,100),  
              col = "turquoise4", 
              width = 1,
              las=2)
      
      renderPlot(CIplot)
    }
    
    else if(input$AboutModel=="aboutTIA"){
      
      TIAplot<- barplot(c(aboutPlotTIA$Accuracy,aboutPlotTIA$Specificity,aboutPlotTIA$Sensitivity),
                       names.arg = c("Accuracy","Specificity","Sensitivity"), 
                       main="About 30 Day Transient Ischemic Attack Risk Model",
                       horiz = F, 
                       ylim=c(0,100), 
                       col = "turquoise4", 
                       width = 1,
                       las=2)
      
      renderPlot(TIAplot)
    }
    
    if(input$AboutModel=="aboutMI"){
      
      MIplot<- barplot(c(aboutPlotMI$Accuracy,aboutPlotMI$Specificity,aboutPlotMI$Sensitivity),
                       names.arg = c("Accuracy","Specificity","Sensitivity"), 
                       main="About 30 Day Myocardial Infarction Risk Model",
                       horiz = F, 
                       ylim=c(0,100),  
                       col = "turquoise4", 
                       width = 1,
                       las=2)
      
      renderPlot(MIplot)
    }
    
    if(input$AboutModel=="aboutRF"){
      
      RFplot<- barplot(c(aboutPlotRF$Accuracy,aboutPlotRF$Specificity,aboutPlotRF$Sensitivity),
                       names.arg = c("Accuracy","Specificity","Sensitivity"), 
                       main="About 30 Day Renal Failure Risk Model",
                       horiz = F, 
                       ylim=c(0,100),  
                       col = "turquoise4", 
                       width = 1,
                       las=2)
      
      renderPlot(RFplot)
    }
    
    if(input$AboutModel=="aboutCDiff"){
      
      CDiffplot<- barplot(c(aboutPlotCDiff$Accuracy,aboutPlotCDiff$Specificity,aboutPlotCDiff$Sensitivity),
                       names.arg = c("Accuracy","Specificity","Sensitivity"), 
                       main="About Clostridium Difficile Risk Model",
                       horiz = F, 
                       ylim=c(0,100), 
                       col = "turquoise4", 
                       width = 1,
                       las=2)
      
      renderPlot(CDiffplot)
    }
    
    if(input$AboutModel=="aboutd90"){
      
      d90plot<- barplot(c(aboutPlotd90$Accuracy,aboutPlotd90$Specificity,aboutPlotd90$Sensitivity),
                       names.arg = c("Accuracy","Specificity","Sensitivity"), 
                       main="About 90 Day Mortaility Risk Model",
                       horiz = F, 
                       ylim=c(0,100),  
                       col = "turquoise4", 
                       width = 1,
                       las=2)
      
      renderPlot(d90plot)
    }
    
    if(input$AboutModel=="aboutInfection"){
      
      d90plot<- barplot(c(aboutPlotInfection$Accuracy,aboutPlotInfection$Specificity,aboutPlotInfection$Sensitivity),
                        names.arg = c("Accuracy","Specificity","Sensitivity"), 
                        main="About Infection Risk Model",
                        horiz = F, 
                        ylim=c(0,100),  
                        col = "turquoise4", 
                        width = 1,
                        las=2)
      
      renderPlot(d90plot)
    }
    
    
    
    
  })
  
  output$aboutPlot<- renderPlot({
    aboutModelPlot()
  })
  
#End of CI Risk Estimate -----------------------------------------------------------------------------
  
# Yearly Statistics Plot on Start Page --------------------------------------------------------------
  

  output$yearlyStatsPlot<- renderPlot({
    
    if(input$yearlyStats=="yearlyTotal"){
      
      ggplot(aes(y=years$total,x=years$year),data=years)+ geom_line(color="darkblue")+geom_point(color="darkblue")+
        labs(title= "THR Surgeries as recorded by the HES in NHS from 2005-2014", x="Year",y="Number Episodes")+
        scale_x_continuous(breaks=seq(2005,2014,1))
    }
    
    else if(input$yearlyStats=="yearlyTIA"){
      ggplot(aes(y=years$TIA,x=years$year),data=years)+ geom_line(color="darkblue")+geom_point(color="darkblue")+
        labs(title= "Percentage of total THR patients in the NHS that had TIA post-surgery from 2005-2014", x="Year",y="Number Episodes")+
        scale_x_continuous(breaks=seq(2005,2014,1))
    }
    
    else if(input$yearlyStats=="yearlyMI"){
      ggplot(aes(y=years$MI,x=years$year),data=years)+ geom_line(color="darkblue")+geom_point(color="darkblue")+
        labs(title= "Percentage of total THR patients in the NHS that had MI post-surgery from 2005-2014", x="Year",y="Number Episodes")+
        scale_x_continuous(breaks=seq(2005,2014,1))
    }
    
    else if(input$yearlyStats=="yearlyRF"){
      ggplot(aes(y=years$RF,x=years$year),data=years)+ geom_line(color="darkblue")+geom_point(color="darkblue")+
        labs(title= "Percentage of total THR patients in the NHS that had RF post-surgery from 2005-2014", x="Year",y="Number Episodes")+
        scale_x_continuous(breaks=seq(2005,2014,1))
    }
    
    else if(input$yearlyStats=="yearlyCDiff"){
      ggplot(aes(y=years$CDiff,x=years$year),data=years)+ geom_line(color="darkblue")+geom_point(color="darkblue")+
        labs(title= "TPercentage of total THR patients in the NHS that had CDiff post-surgery from 2005-2014", x="Year",y="Number Episodes")+
        scale_x_continuous(breaks=seq(2005,2014,1))
    }
    
    else if(input$yearlyStats=="yearlyd90"){
      ggplot(aes(y=years$died90,x=years$year),data=years)+ geom_line(color="darkblue")+geom_point(color="darkblue")+
        labs(title= "Percentage of total THR patients in the NHS that died 90 days post-surgery from 2005-2014", x="Year",y="Number Episodes")+
        scale_x_continuous(breaks=seq(2005,2014,1))
    }
    
    else if(input$yearlyStats=="yearlyCI"){
      ggplot(aes(y=years$CI,x=years$year),data=years)+ geom_line(color="darkblue")+geom_point(color="darkblue")+
        labs(title= "Percentage of total THR patients in the NHS that had CI post-surgery from 2005-2014", x="Year",y="Number Episodes")+
        scale_x_continuous(breaks=seq(2005,2014,1))
    }
    
    else if(input$yearlyStats=="yearlyInfection"){
      ggplot(aes(y=years$infection,x=years$year),data=years)+ geom_line(color="darkblue")+geom_point(color="darkblue")+
        labs(title= "Percentage of total THR patients in the NHS that had Infection post-surgery from 2005-2014", x="Year",y="Number Episodes")+
        scale_x_continuous(breaks=seq(2005,2014,1))
    }
  })  
  
  
# End of Yearly Statistics Plot on Start Page --------------------------------------------------------------
 
# NN Model Performance Plot---------------------------------------------------------
 
  output$modelPerformancePlot<-renderPlot({
  ggplot(data=modelPerformance,aes(x=modelPerformance$complication, y=modelPerformance$value, fill=modelPerformance$property))+
    geom_bar(stat = "identity" ,position=position_dodge()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ylim(0,100)+
    xlab("Complication")+
    ylab("Percent")+
    labs(fill="Model Property")
  })
  
#End of  NN Model Performance Plot---------------------------------------------------------

# Model comparison plots------------------------------------------------------------------- 
  output$modelComparisonPlot<- renderPlot({
    
    if(input$modelCompare=="compareTIA"){
      
      ggplot(data=modelCompareTIA, aes(x=modelCompareTIA$Acc,y=modelCompareTIA$Sen,size=3, color=modelCompareTIA$model))+ 
        scale_size(guide="none")+
        geom_point() + 
        xlim(40,100)+ylim(0,60)+
        geom_vline(xintercept = 70) + 
        geom_hline(yintercept = 30) +
        xlab("Accuracy")+
        ylab("Sensitivity")+
        labs(color="Model")
    }
    
    else if(input$modelCompare=="compareMI"){
      
      ggplot(data=modelCompareMI, aes(x=modelCompareMI$Acc,y=modelCompareMI$Sen,size=3, color=modelCompareMI$model))+ 
        scale_size(guide="none")+
        geom_point() + 
        xlim(40,100)+ylim(40,70)+
        geom_vline(xintercept = 70) + 
        geom_hline(yintercept = 55) +
        xlab("Accuracy")+
        ylab("Sensitivity")+
        labs(color="Model")
    }
    
    else if(input$modelCompare=="compareRF"){
      ggplot(data=modelCompareRF, aes(x=modelCompareRF$Acc,y=modelCompareRF$Sen,size=3, color=modelCompareRF$model))+ 
        scale_size(guide="none")+
        geom_point() + 
        xlim(40,100)+ylim(45,65)+
        geom_vline(xintercept = 70) + 
        geom_hline(yintercept = 55) +
        xlab("Accuracy")+
        ylab("Sensitivity")+
        labs(color="Model")
    }
    
    else if(input$modelCompare=="compareCDiff"){
      ggplot(data=modelCompareCDiff, aes(x=modelCompareCDiff$Acc,y=modelCompareCDiff$Sen,size=3, color=modelCompareCDiff$model))+ 
        scale_size(guide="none")+
        geom_point() + 
        xlim(40,100)+ylim(0,50)+
        geom_vline(xintercept = 70) + 
        geom_hline(yintercept = 25) +
        xlab("Accuracy")+
        ylab("Sensitivity")+
        labs(color="Model")
    }
    
    else if(input$modelCompare=="compared90"){
      ggplot(data=modelCompared90, aes(x=modelCompared90$Acc,y=modelCompared90$Sen,size=3, color=modelCompared90$model))+ 
        scale_size(guide="none")+
        geom_point() + 
        xlim(40,100)+ylim(40,70)+
        geom_vline(xintercept = 70) + 
        geom_hline(yintercept = 55) +
        xlab("Accuracy")+
        ylab("Sensitivity")+
        labs(color="Model")
    }
    
    else if(input$modelCompare=="compareCI"){
      ggplot(data=modelCompareCI , aes(x=modelCompareCI$Acc,y=modelCompareCI$Sen,size=3, color=modelCompareCI$model))+ 
        scale_size(guide="none")+
        geom_point() + 
        xlim(40,100)+ylim(30,60)+
        geom_vline(xintercept = 70) + 
        geom_hline(yintercept = 45) +
        xlab("Accuracy")+
        ylab("Sensitivity")+
        labs(color="Model")
    }
    
    else if(input$modelCompare=="compareInfection"){
      ggplot(data=modelCompareInfection , aes(x=modelCompareInfection$Acc,y=modelCompareInfection$Sen,size=3, color=modelCompareInfection$model))+ 
        scale_size(guide="none")+
        geom_point() + 
        xlim(40,100)+ylim(0,50)+
        geom_vline(xintercept = 70) + 
        geom_hline(yintercept = 25) +
        xlab("Accuracy")+
        ylab("Sensitivity")+
        labs(color="Model")
    }
      
      
  })
#End of Model Comparison plots  
})
