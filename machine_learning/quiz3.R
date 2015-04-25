# quiz 3

library(caret)
library(rattle)


# q1
#############################

library(AppliedPredictiveModeling)
data(segmentationOriginal)
# 1. Subset the data to a training set and testing set based on the Case variable in the data set. 
training <- segmentationOriginal[segmentationOriginal$Case=='Train',]; 
testing <- segmentationOriginal[segmentationOriginal$Case=='Test',]; 

training$Case <- NULL;

# 2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables 
# and default caret settings. 
set.seed(125);
rpartFit <- train(Class~.,method='rpart',data=training);
print(rpartFit$finalModel);
fancyRpartPlot(rpartFit$finalModel)

# 3. In the final model what would be the final model prediction for cases with the following variable values:
# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 PS
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 WS
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 PS
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 PS
# PS WS PS cant_tell

##############
# Q2

# If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample 
# (test set) accuracy smaller or bigger? If K is small is the variance in the estimate of 
# out-of-sample (test set) accuracy smaller or bigger. Is K large or small in leave one out cross
# validation?

# ans: The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to two.

###############
# Q3
# These data contain information on 572 different Italian olive oils from multiple regions in Italy.
# Fit a classification tree where Area is the outcome variable. Then predict the value of area for 
# the following data frame using the tree command with all defaults

library(pgmm)
data(olive)
olive = olive[,-1]

rpart2Fit <- train(Area~.,method='rpart2',data=olive)
fancyRpartPlot(rpart2Fit$finalModel)
newdata = as.data.frame(t(colMeans(olive)))
predict(rpart2Fit, newdata)

# What is the resulting prediction? Is the resulting prediction strange? Why or why not?

# ans: 2.875. It is strange because Area should be a qualitative variable - but tree is 
# reporting the average value of Area as a numeric variable in the leaf predicted for newdata

# Q4
######################

# Load the South Africa Heart Disease Data and create training and test sets with the following code:
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

# Then set the seed to 13234 and fit a logistic regression model (method="glm", be sure to specify 
# family="binomial") with Coronary Heart Disease (chd) as the outcome and age at onset, current
# alcohol consumption, obesity levels, cumulative tabacco, type-A behavior, and low density 
# lipoprotein cholesterol as predictors. Calculate the misclassification rate for your model using 
# this function and a prediction on the "response" scale:

set.seed(13234)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
glmTrain <- train()
# What is the misclassification rate on the training set? What is the misclassification rate on the
# test set?
