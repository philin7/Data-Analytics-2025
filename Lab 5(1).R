library(readr)
library(ggplot2)
library(e1071)
library(caret)
library(class) 

# read dataset
setwd("C:/Users/15184/Downloads/Spring 2025/Data Analytics/Lab 5/")
wine <- read.csv("wine.csv") 
wine <- wine[-which(wine$Class==3),]


# dataset <- wine
# dataset$Type <- as.factor(dataset$Type)

## column names
names(wine)

## split train/test
train.indexes <- sample(nrow(wine),0.75*nrow(wine))

train <- wine[train.indexes,]
test <- wine[-train.indexes,]

x <- wine[,2:4] 
y <- as.factor(as.matrix(wine[,1]))


svr.mod0 <- svm(as.factor(Class) ~ (Total.phenols + Color.intensity + Ash + Magnesium + Flavanoids + Alcohol + Proline), train, kernel ="linear")

summary(svr.mod0)

svr.pred <- predict(svr.mod0, test)
##Accuracy
cm = as.matrix(table(Actual = test$Class, Predicted = svr.pred))
cm
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

####################

svr.mod0 <- svm(as.factor(Class) ~ (Total.phenols + Color.intensity + Ash + Magnesium + Flavanoids + Alcohol + Proline), train, kernel ="radial")

summary(svr.mod0)

svr.pred <- predict(svr.mod0, test)
##Accuracy
cm = as.matrix(table(Actual = test$Class, Predicted = svr.pred))
cm
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

##############################

# simple estimate of k
k = round(sqrt(nrow(wine)))

k <- k-1

knn.predicted <- knn(train = train, test = test, cl = train$Class, k = 11)

##Accuracy
#cm = as.matrix(table(Actual = train$Class, Predicted = knn.predicted))

cm <- table(knn.predicted, test$Class, dnn=list('predicted','actual'))
cm
# calculate classification accuracy
sum(diag(cm))/length(test$Class)

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

###● Compare the performance of the 2 models (Precision, Recall, F1)
## SVM has higher Precision Recall and F1 values (all 1s) so it performed better than the knn model

