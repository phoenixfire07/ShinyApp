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

#Random Forest Classification
library(randomForest)
classifier<- randomForest(x= training[-18], 
                            y= training$died90, 
                            ntree=150
)

# Predicting the Test set results based on the explanatory variables
y_pred= predict(classifier,newdata = testing[-18])

# Making the Confusion Matrix
library(caret)
confusionMatrix(as.factor(y_pred), as.factor(testing$died90))







