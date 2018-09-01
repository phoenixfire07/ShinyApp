# Import dataset
d<-read.csv(file.choose())

# Leave only the response variable of interest
d<-d[,1:18]

# Divide the dataset into training set (70%) and test set (30%)
set.seed(222)
div<- sample(2,nrow(d),replace = T, prob = c(0.7,0.3))
training<-d[div==1,]
testing<-d[div==2,]

# Import data resampling library
library(DMwR)

# Smote funtion required features to be factored
training$died90= factor(training$died90, levels = c(0,1))

# Smote funtion resamples data for 1:1, majority:minority ratio
training<- SMOTE(died90~., data=training,perc.over =550,k=5, perc.under = 120, learner=NULL)

# Type conversion back to numeric for input into models
training$died90<-as.numeric(as.character(training$died90))

# Generating Neural Network
library(neuralnet)
NN = neuralnet(died90~ Age+Sex+CIHD+HyperT+HypoT+IDDM+NIDDM+HCD+COPD+D+Alz+Ost+HC+HCVA+DU+HT+AF, 
               data=training, 
               hidden=c(3),
               stepmax = 1e9,
               threshold = 0.5,
               lifesign="full"
)

# Predicing the probability of complications in the test set
pred<-compute((NN),testing[,-c(18)])
pred<-pred$net.result
y_pred<- ifelse(pred>0.5,1,0)

# Get confusion matrix
library(caret)
confusionMatrix(as.factor(y_pred), as.factor(testing$died90))

# Generating Variable Importance
library(NeuralNetTools)
olden(NN)

# Generating Neural Network Plot
plot(NN)
  
# Export network with best performance
saveRDS(NN, file = "died90.rda")









