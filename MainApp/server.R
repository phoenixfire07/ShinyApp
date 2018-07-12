
library(shiny)
library(neuralnet)
library(caret)


shinyServer(function(input, output) {
  # 
  # NN2<- readRDS("NN/NNdied90.rda")
  # 
  # plotNN2<- eventReactive(input$button,{
  #   renderPlot(plot(NN2))
  # })

  output$plot <- renderPlot({
    plot(NN2)
  })
  
  
  # textWait<- eventReactive(input$button,{
  #   "This may take a couple of minutes."
  # })
  # 
  # output$wait <- renderText({
  #   textWait()
  # })
  
 

  
  
  # observeEvent(input$button,{
  #   output$text<-"poop"
  # })
  # # output$text<-renderText({
  # #   input$COM
  # })
  
})
