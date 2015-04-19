library(ISLR);
library(ggplot2);
library(caret);
data(Wage)
summary(Wage)
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); 
dim(testing)
dim(Wage)
featurePlot(x=training[,c("age","education","jobclass")], y = training$wage, plot="pairs")
qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass,data=training)
# regression smoothers
qplot(age,wage,colour=education,data=training) +  geom_smooth(method='lm',formula=y~x)

# cut a table into intervals
library(Hmisc)
cutWage <- cut2(training$wage,g=3)
table(cutWage)

# box plot with cut2
qplot(cutWage,age, data=training,fill=cutWage,geom=c("boxplot"))

p1 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot"))
p2 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot","jitter"))
#install.packages('gridExtra')
library(gridExtra)
grid.arrange(p1,p2,ncol=2)

t1 <- table(cutWage,training$jobclass)
t1
prop.table(t1,1)

qplot(wage,colour=education,data=training,geom="density")


# 14: preprocessing
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve,main="",xlab="ave. capital run length")
mean(training$capitalAve)
sd(training$capitalAve)

trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve  - mean(trainCapAve))/sd(trainCapAve) 
mean(trainCapAveS)
sd(trainCapAveS)

testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve  - mean(trainCapAve))/sd(trainCapAve) 
mean(testCapAveS)
sd(testCapAveS)

preObj <- preProcess(training[,-58],method=c("center","scale"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
mean(trainCapAveS)

testCapAveS <- predict(preObj,testing[,-58])$capitalAve
mean(testCapAveS)

sd(testCapAveS)

set.seed(32343)
modelFit <- train(type ~.,data=training,
                  preProcess=c("center","scale"),method="glm")
modelFit

preObj <- preProcess(training[,-58],method=c("BoxCox"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)

set.seed(13343)

# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA

# Impute and standardize
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj,training[,-58])$capAve

# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])


# lecture 15 covariate
library(kernlab);data(spam)
spam$capitalAveSq <- spam$capitalAve^2

library(ISLR); 
library(caret);
data(Wage);
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,];
testing <- Wage[-inTrain,]
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass,data=training)
head(predict(dummies,newdata=training))
# some covariates have very little value, e.g. 'region' or 'sex'
# if you can identify these you can take them out of model because they 
# don't add anything
nsv <- nearZeroVar(training,saveMetrics=TRUE)
nsv # region and sex have no value

# if you want to fit curvy lines instead of straight
# use the splines package
library(splines)
bsBasis <- bs(training$age,df=3) 
bsBasis # age, age^2, age^3

lm1 <- lm(wage ~ bsBasis,data=training)
plot(training$age,training$wage,pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)
predict(bsBasis,age=testing$age)

# Level 1 feature creation (raw data to covariates)
# Science is key. Google "feature extraction for [data type]"
# Err on overcreation of features
# In some applications (images, voices) automated feature creation is possible/necessary
# http://www.cs.nyu.edu/~yann/talks/lecun-ranzato-icml2013.pdf
# Level 2 feature creation (covariates to new covariates)
# The function preProcess in caret will handle some preprocessing.
# Create new covariates if you think they will improve fit
# Use exploratory analysis on the training set for creating them
# Be careful about overfitting!
#  preprocessing with caret
# If you want to fit spline models, use the gam method in the caret package which allows smoothing of multiple variables.