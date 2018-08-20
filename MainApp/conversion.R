library(neuralnet)
d<-read.csv(file.choose())
d1<-d


d1<-d1[,1:18]
d1<-d1[,-18]


d1$Age<-(d1$Age-min(d1$Age))/(max(d1$Age)-min(d1$Age))

d1$Sex<-(d1$Sex-min(d1$Sex))/(max(d1$Sex)-min(d1$Sex))

set.seed(222)

div<- sample(2,nrow(d1),replace = T, prob = c(0.7,0.3))
training<-d1[div==1,]
testing<-d1[div==2,]

library(DMwR)
# 
# trainingSmote<-training
# 
# trainingSmote$MIin30= factor(trainingSmote$died90, levels = c(0,1))
# 
# 
# smote<- SMOTE(died90~., data=trainingSmote ,perc.over =550,k=5, perc.under = 138, learner=NULL)
# 
# table(smote$died90)
# 
# trainingTest<-smote
# 
# trainingTest$died90<-as.numeric(as.character(smote$died90))


# library(caret)

NN<- readRDS("NN/died90.rda")

pred<-compute((NN),testing[,-18])
p1<-pred$net.result



pred1<- ifelse(p1>0.95,1,0)

died90common<-99.713
died90rare<-0.286
startingPrediction<-.95
rareChange<- 50/died90rare
commonChange<-50/died90common
rareAdjusted<-startingPrediction/rareChange
commonAdjusted<-(1-startingPrediction)/commonChange
finalprediction=100*(rareAdjusted/(rareAdjusted+commonAdjusted))

finalprediction



# predictedValue<-compute((NN),testing[,-18])

# died90common<-99.713
# died90rare<-0.286
# startingPrediction<-as.numeric(predictedValue$net.result)
# rareChange<- 50/died90rare
# commonChange<-50/died90common
# rareAdjusted<-startingPrediction/rareChange
# commonAdjusted<-(1-startingPrediction)/commonChange
# finalprediction=100*(rareAdjusted/(rareAdjusted+commonAdjusted))










confusionMatrix(as.factor(pred1), as.factor(testing$TIAin30))








X <- rbinom(133463, 1,0.13421697)
confusionMatrix(as.factor(X), as.factor(testing$TIAin30))
