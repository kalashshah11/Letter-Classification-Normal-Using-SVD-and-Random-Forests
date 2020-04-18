library(e1071)
library(MASS)
library(ggord)
library(corrplot)
library(xlsx)
library(randomForest)
require(foreign)
require(nnet)
require(ggplot2)
library(useful)
require(reshape2)
set.seed(7)
origdata <- read.table("letter-recognition.data", header=FALSE,sep=",")


#No Missing Values So No Preprocessing Required

#Breaking Data Into Train and Test
dt = sort(sample(nrow(origdata), nrow(origdata)*.7))
mydata.train <- origdata[dt,]
mydata.test <- origdata[-dt,]
#Original Test Data and Train data Separated

# 1. Linear Discriminant Analysis

linear <- lda(V1~., mydata.train)
linear
summary(linear)

# Visualising separation for training data
ggord(linear, mydata.train$V1)

#Accuracy on training data
p1 <- predict(linear, mydata.train)$class
tab1 <- table(Predicted = p1, Actual = mydata.train$V1)
sum(diag(tab1))/sum(tab1)

#Accuracy on test data
p2 <- predict(linear, mydata.test)$class
tab2 <- table(Predicted = p2, Actual = mydata.test$V1)
sum(diag(tab2))/sum(tab2)

#BarPlot for training data
report1<-rbind(summary(p1),summary(mydata.train$V1))
barplot(as.matrix(report1), main="Data Prediction 1 : Training", ylab = "Count", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col = rainbow(2))

#BarPlot for test data
report2<-rbind(summary(p2),summary(mydata.test$V1))
barplot(as.matrix(report2), main="Data Prediction 2 : Test", ylab = "Count", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col = rainbow(2))

# 2. Principal Component Analysis

#Visualizing dependency of variables with each other
crp1 = cor(origdata[sapply(origdata,is.numeric)],method="pearson")
corrplot(crp1,method="number")
corrplot(crp1,method="pie")

#Generating Vectors of PCA
prin_comp <- prcomp(mydata.train[,2:ncol(mydata.train)], scale. = T) 
summary(prin_comp)
plot(prin_comp, col=rgb(0.8,0.1,0.1,0.6))

#biplot(prin_comp, scale = 0)
prin_comp$x

#Taking Training Data and Joining with Labelled Letters
train.data <- data.frame(Predictions = mydata.train$V1, prin_comp$x)
train.data <- train.data[,1:15]
modelLF <- svm(Predictions~., data=train.data , kernel ="linear", cost=100, scale=FALSE)

#Correlation between the principal components
crp2 = cor(train.data[sapply(train.data,is.numeric)],method="pearson")
corrplot(crp2,method="number")
corrplot(crp2,method="pie")

#Testing Model for Training Data
trainDataPredictionLF = predict(modelLF,prin_comp$x[,1:14])

xtabLF <- table(mydata.train$V1, trainDataPredictionLF)
accuracy1LF <- sum(diag(xtabLF)) / sum(xtabLF)


xLF <- compare.list(trainDataPredictionLF,mydata.train$V1)
summary(xLF)
report<-rbind(summary(trainDataPredictionLF),summary(mydata.train$V1))
barplot(as.matrix(report), main="Data Prediction 3 : Training", ylab = "Count", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col = rainbow(2))

#Changing Test Data using PCA vectors of Training Data
test.data <- predict(prin_comp, newdata = mydata.test[,2:ncol(mydata.test)])
test.data <- as.data.frame(test.data)
test.data <- test.data[,1:14]

#Using TestData with Model
testDataPredictionLF = predict(modelLF,test.data)

ytabLF <- table(mydata.test$V1, testDataPredictionLF)
accuracy2LF <- sum(diag(ytabLF)) / sum(ytabLF)

yLF <- compare.list(testDataPredictionLF,mydata.test$V1)
summary(yLF)

report<-rbind(summary(testDataPredictionLF),summary(mydata.test$V1))
barplot(as.matrix(report), main="Data Prediction 4 : Test", ylab = "Count", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col = rainbow(2))

# 3. Random Forest

t1 <- mydata.train
t2 <- mydata.test
model <- randomForest(V1 ~ V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17,data=t1,ntree=10,importace=TRUE)

pred <- predict(model,newdata=t2)
tab3 <- table(pred,t2$V1)
sum(diag(tab3))/sum(tab3)

pred <- predict(model,newdata=t1)
tab4 <- table(pred,t1$V1)
sum(diag(tab4))/sum(tab4)