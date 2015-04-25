# quiz2 

# q1 50% samples
# adData = data.frame(diagnosis,predictors)
# testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
# training = adData[-testIndex,]
# testing = adData[testIndex,]

# q2
# install.packages('AppliedPredictiveModeling')
# Make a plot of the outcome (CompressiveStrength) versus the index of the samples. 
# Color by each of the variables in the data set (you may find the cut2() function in the 
# Hmisc package useful for turning continuous covariates into factors). 
# What do you notice in these plots?
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

library(Hmisc)
# these rows the ceemtn type changes towards the end
qplot(1:nrow(concrete), concrete$CompressiveStrength, color=cut2(concrete$Cement))
qplot(1:nrow(concrete), concrete$CompressiveStrength, color=cut2(concrete$BlastFurnaceSlag))
qplot(1:nrow(concrete), concrete$CompressiveStrength, color=cut2(concrete$Age))
qplot(1:nrow(concrete), concrete$CompressiveStrength, color=cut2(concrete$FlyAsh))
qplot(1:nrow(concrete), concrete$CompressiveStrength, color=cut2(concrete$Water))
qplot(1:nrow(concrete), concrete$CompressiveStrength, color=cut2(concrete$Superplasticizer))
qplot(1:nrow(concrete), concrete$CompressiveStrength, color=cut2(concrete$CoarseAggregate))
qplot(1:nrow(concrete), concrete$CompressiveStrength, color=cut2(concrete$FineAggregate))

# there is definitely a step pattern but age doesn't seem to have anything to do with it
# nope: The data show a step like pattern that is perfectly explained by the Age variable.
# nope: The data show a step like pattern that is perfectly explained by the Age variable so there may be a variable missing.
# There is a step-like pattern in the plot of outcome versus index in the training set that isn't explained by any of the predictor variables so there may be a variable missing.
# conclusion: step pattern but we're missing a variable

# Q3
# Make a histogram and confirm the SuperPlasticizer variable is skewed. Normally you might use the 
# log transform to try to make the data more symmetric. Why would that be a poor choice for this 
# variable?
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
hist(training$Superplasticizer)
hist(log(training$Superplasticizer+1))

# There are a large number of values that are the same and even if you took the
#log(SuperPlasticizer + 1) they would still all be identical so the distribution would not be 
# symmetric.
# nope: this isn't just skewness because of the huge number of 0 values

# Q4 
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
# Find all the predictor variables in the training set that begin with IL. Perform principal 
# components on these variables with the preProcess() function from the caret package. Calculate
# the number of principal components needed to capture 90% of the variance. How many are there?
ilCols = grep('^IL', names(training))
ilVars <- training[,ilCols]
preProc <- preProcess(ilVars,method="pca",thresh=.9)
preProc # needed 9 components to capture 90% of the variance

# Q5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
# Create a training data set consisting of only the predictors with variable names beginning with IL
# and the diagnosis. Build two predictive models, one using the predictors as they are and one using
# PCA with principal components explaining 80% of the variance in the predictors. 
# Use method="glm" in the train function. What is the accuracy of each method in the test set? 
# Which is more accurate?
cols = grep('(^IL|diagnosis)', names(adData))
training = adData[ inTrain,cols]
testing = adData[-inTrain,cols]

# first w/o pca. 70% accurate
modelFit <- train(training$diagnosis ~ .,method="glm",preProcess="pca",data=training)
confusionMatrix(testing$diagnosis,predict(modelFit,testing))

# now with preprocL 72%
preProc <- preProcess(training[,-1],method="pca",thresh=.8)
trainPC <- predict(preProc,training[,-1])
modelFit <- train(training$diagnosis ~ .,method="glm",data=trainPC)

testPC <- predict(preProc,testing[,-1])
confusionMatrix(testing$diagnosis,predict(modelFit,testPC))

# guess: 
# non pca: 65%
# pca: 72%
