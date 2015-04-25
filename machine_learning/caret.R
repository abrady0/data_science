# 
library(caret)
library(kernlab)
data(spam)

inTrain <- createDataPartition(
  y=spam$type, 
  p=0.75,  # 75% data in training set
  list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
set.seed(32343)
modelFit <- train(type ~., data=training, method="glm")
predictions <- predict(modelFit,newdata=testing)
predictions
confusionMatrix(predictions,testing$type)

# train options
args(train.default)
# weights
# metric:
# - for continuous: "RMSE" : root mean square for continuous, RSquared: from regression for linear agreement
# - catagorical: Accuracy = fraction correct, Kappa = a measure of concordance
# trControl = trainControl()
# - method = "boot" or "cross validate"
# - number = how many times to do this
# - repeat = repeat the whole process
# - p = 0.75 : % to use
# - horizon : number of time points predicting
# - savePredictions : return predictions
# - preProcOptions
# - allowParallel = T

# trainControl resampling:
# method:
# - boot
# - boot632 : reduce bias of resample
# - cv: cross validation
# - repeatedcv: repeated cross validation
# - LOOCV = leave one out
# number:
# - for boot/cross validation
# repeats:

# setting the seed
# - often useful to set an overall seed
# - you can also set a seed for each resample
# - seeding each resample is useful for parallel fits

