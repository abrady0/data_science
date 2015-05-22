# quiz4
library(caret)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

# Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833. 
# Fit (1) a random forest predictor relating the factor variable y to the remaining variables and (2) a boosted 
# predictor using the "gbm" method. Fit these both with the train() command in the caret package. 
# What are the accuracies for the two approaches on the test data set? 
# What is the accuracy among the test set samples where the two methods agree?
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
gbmTrain <- train(y~.,method='gbm',data=vowel.train)
rfTrain <- train(y~.,method='rf',data=vowel.train)
rfPred <- predict(rfTrain, newdata = vowel.test)
gbmPred <- predict(gbmTrain, newdata=vowel.test)
confusionMatrix(rfPred, vowel.test$y)
confusionMatrix(gbmPred, vowel.test$y)
summary(rfPred)
summary(gbmPred)
agreeIdx <- rfPred == gbmPred
agreePred <- rfPred[agreeIdx]
testPred <- vowel.test[agreeIdx,]
confusionMatrix(rfPred, vowel.test$y)$overall[[1]]
confusionMatrix(gbmPred, vowel.test$y)$overall[[1]]
confusionMatrix(agreePred, testPred$y)$overall[[1]]
# rf .606, gb .53, agree .657

# Question 2
################

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Set the seed to 62433 and predict diagnosis with all the other variables using a random forest ("rf"), boosted trees ("gbm") 
# and linear discriminant analysis ("lda") model. Stack the predictions together using random forests ("rf"). What is the resulting 
# accuracy on the test set? Is it better or worse than each of the individual predictions?
set.seed(62433)
rfTrain <- train(diagnosis~.,method='rf',data=training)
gbmTrain <- train(diagnosis~.,method='gbm',data=training)
ldaTrain <- train(diagnosis~.,method='lda',data=training)

rfPred <- predict(rfTrain, newdata=testing)
gbmPred <- predict(gbmTrain, newdata=testing)
ldaPred <- predict(ldaTrain, newdata=testing)
stackData <- data.frame(rfPred,gbmPred,ldaPred,diagnosis=testing$diagnosis)
#rfPred <- predict(rfTrain, newdata=training)
#gbmPred <- predict(gbmTrain, newdata=training)
#ldaPred <- predict(ldaTrain, newdata=training)
#stackData <- data.frame(rfPred,gbmPred,ldaPred,diagnosis=training$diagnosis)

stackTrain <- train(diagnosis~.,method='rf',data=stackData)
stackPred <- predict(stackTrain,newdata=stackData)
confusionMatrix(rfPred,testing$diagnosis)$overall
confusionMatrix(gbmPred,testing$diagnosis)$overall
confusionMatrix(ldaPred,testing$diagnosis)$overall
confusionMatrix(stackPred,testing$diagnosis)$overall
# Stacked Accuracy: 0.79 is better than random forests and lda and the same as boosting.

# gbm and stacked are basically the same


# question 3
##############
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

#Set the seed to 233 and fit a lasso model to predict Compressive Strength. 
# Which variable is the last coefficient to be set to zero as the penalty increases? (Hint: it may be useful to look up ?plot.enet).
set.seed(233)
lassoTrain <- train(CompressiveStrength~.,method='lasso',data=training)
summary(lassoTrain)
plot(lassoTrain$finalModel) # course aggregate or cement?
# not course aggregate
# cement?

# question 4
################
library(lubridate)  # For year() function below
install.packages('ts')
dat = read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

# Fit a model using the bats() function in the forecast package to the training time series. 
# Then forecast this model for the remaining time points. 
# For how many of the testing points is the true value within the 95% prediction interval bounds?
batsTrain <- train()

# 96%

# question 5
################
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

# Set the seed to 325 and fit a support vector machine using the e1071 package to predict Compressive Strength using the 
# default settings. Predict on the testing set. What is the RMSE?
set.seed(325)
mod <- svm(CompressiveStrength ~ ., data = training)
pred <- predict(mod, testing)
# not 107.447
# 6.93?
mean((pred-testing$CompressiveStrength)^2) 

