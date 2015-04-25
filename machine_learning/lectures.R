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

# 16 Preprocessing with Principal Components Analysis (PCA)
###########################################

library(caret); 
library(kernlab);
data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)

smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

# we can automate this 'combining' of variables in two ways:
# SVD
# If X is a matrix with each variable in a column and each observation in a row then the SVD is a "matrix decomposition"
# X=UDVT
#where the columns of U are orthogonal (left singular vectors), the columns of V are orthogonal (right singluar vectors) and D is a diagonal matrix (singular values).
#PCA
# The principal components are equal to the right singular values if you first scale (subtract the mean, divide by the standard deviation) the variables.

smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])
prComp$rotation # how transformed

# on more variables
typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1)) # log10 to reduce skew
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")

# prCom$x[,1] explains the most variability, 2, the second most, etc.
head(prComp$x)

# in caret
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(training$type ~ .,method="glm",data=trainPC)

testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))

# alternative: preProcess with pca directly
# above is just to show you what is under the hood
modelFit <- train(training$type ~ .,method="glm",preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))

# Most useful for linear-type models
# Can make it harder to interpret predictors
# Watch out for outliers!
#  Transform first (with logs/Box Cox)
# Plot predictors to identify problems


# Regression
###########################################

library(caret);
data(faithful); 
set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting,
                               p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
head(trainFaith)

# fit a linear model
lm1 <- lm(eruptions ~ waiting,data=trainFaith)
summary(lm1)

# prediction intervals
pred1 <- predict(lm1,newdata=testFaith,interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",,col=c(1,2,2),lty = c(1,1,1), lwd=3)

# with caret
modFit <- train(eruptions ~ waiting,data=trainFaith,method="lm")
summary(modFit$finalModel)

# Notes and further reading
# Regression models with multiple covariates can be included
# Often useful in combination with other models
# Elements of statistical learning : http://www-stat.stanford.edu/~tibs/ElemStatLearn/
# Modern applied statistics with S : http://www.amazon.com/Modern-Applied-Statistics-W-N-Venables/dp/0387954570
# Introduction to statistical learning : http://www-bcf.usc.edu/~gareth/ISL/

# 18 regression with covariates
library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage <- subset(Wage,select=-c(logwage))
summary(Wage)
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)
featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")
qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass,data=training)
qplot(age,wage,colour=education,data=training)

modFit<- train(wage ~ age + jobclass + education,
               method = "lm",data=training)
finMod <- modFit$finalModel
print(modFit)
# how did our fitted values do? still some outliers, though the
# line is pretty good
plot(finMod,1,pch=19,cex=0.5,col="#00000010")

# are the outliers explained by race?
qplot(finMod$fitted,finMod$residuals,colour=race,data=training)

# trend with respect to row numbers, shouldn't happen!
# indicates missing data, i.e. a time relationship you didn't capture
plot(finMod$residuals,pch=19)

# year data was collected
# note: can't go back and modify training model from this! you're influencing it
pred <- predict(modFit, testing)
qplot(wage,pred,colour=year,data=testing)

# 
modFitAll<- train(wage ~ .,data=training,method="lm")
pred <- predict(modFitAll, testing)
qplot(wage,pred,data=testing)

# summary: often useful in combination with other models

