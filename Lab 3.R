## read abalonedata
setwd("C:/Users/Nyah/Downloads/Data Analytics/Lab 3")
abalonedata <- readxl::read_excel("abalone_abalonedata.xlsx")
View(abalonedata)

## add new column age.group with 3 values based on the number of rings 
abalonedata$age.group <- cut(abalonedata$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))


## alternative way of setting age.group
# abalonedata$age.group[abalonedata$rings<=8] <- "young"
# abalonedata$age.group[abalonedata$rings>8 & abalonedata$rings<=11] <- "adult"
# abalonedata$age.group[abalonedata$rings>11 & abalonedata$rings<=35] <- "old"

# 1st knn model

#load necessary libraries
library(class)
library(caret)

knn.predicted <- knn(train = abalonedata[,2:4], test = abalonedata[,2:4], cl = abalonedata$age.group, k = 13)

# create contingency table, i.e. confusion matrix 
contingency.table <- table(knn.predicted, abalonedata$age.group, dnn=list('predicted','actual'))

contingency.table

# calculate classification accuracy
sum(diag(contingency.table))/length(abalonedata$age.group)

# 2nd knn model

knn.predicted <- knn(train = abalonedata[,5:8], test = abalonedata[,5:8], cl = abalonedata$age.group, k = 13)

# create contingency table/ confusion matrix 
contingency.table <- table(knn.predicted, abalonedata$age.group, dnn=list('predicted','actual'))

contingency.table

# calculate classification accuracy
sum(diag(contingency.table))/length(abalonedata$age.group)

##Based on the accuracy scores the 2nd model is preferred (~60% correctly classified vs ~70% correctly classified)

########################################################

# make k equal to the square root of 4176, the number of observations in the
training set.

sqrt(4176)

#answer is ~ 65
# list of k
k.list <- c(57,59,61,63,65,67,69) 

# empty list for accuracy
accuracy.list <- c()

# loop: train&predict model for each k, compute accuracy and append it to list
for (k in k.list) {
  
  knn.predicted <- knn(train = abalonedata[,5:8], test = abalonedata[,5:8], cl = abalonedata$age.group, k = k)
  
  contingency.table <- table(knn.predicted, abalonedata$age.group, dnn=list('predicted','actual'))
  
  accuracy <- sum(diag(contingency.table))/length(abalonedata$age.group)
  
  accuracy.list <- c(accuracy.list,accuracy)
  
}


# plot acccuracy with k, limiting values of y axis between .9 & 1
plot(k.list,accuracy.list,type = "b", ylim = c(.65,.7))

###############################################################


Exercise 2 

## run kmeans
k = 19 # when multiple nn models were evaluated and trained, k=19 had highest accuracy
iris.km <- kmeans(abalonedata[,5:8], centers = k)


## get and plot clustering output 

assigned.clusters <- as.factor(abalonedata$age.group)

ggplot(abalonedata, aes(x = whole_weight, y = shucked_wieght, colour = as.factor(assigned.clusters))) +
  geom_point()

abalonedata.km <- kmeans(abalonedata[, 5:8], centers = k)

## WCSS: total within cluster sum of squares
abalonedata.km$tot.withinss

abalonedata.km$cluster


## run tests with multiple k values and plot WCSS
k.list <- c(2,3,4,5,6)

wcss.list <- c()

for (k in k.list) {
  
  abalonedata.km <- kmeans(abalonedata[,5:8], centers = k)
  
  wcss <- abalonedata.km$tot.withinss
  
  wcss.list <- c(wcss.list,wcss)
  
  ## get and plot clustering output 
  assigned.clusters <- as.factor(abalonedata.km$cluster)
  
  ggplot(abalonedata, aes(x = whole_weight, y = shucked_wieght, colour = assigned.clusters)) +
    geom_point()
  
}

plot(k.list,wcss.list,type = "b")


### based on the plot, approximately k= is the most accurate k-value to use


