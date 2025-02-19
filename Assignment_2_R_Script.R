library("ggplot2")
library("readr")
install.packages("readxl")
library(readxl)

## read dataset
setwd("C:/Users/Nyah/Downloads/Data Analytics/Assignment 2")
dataset2 <- readxl::read_excel("epi_results_2024_pop_gdp.xlsx")
View(dataset2)
attach(dataset2) #with this commaned all column names in dataset2 are now available directly without needing to reference dataset2$column_name.

#Chosen Region 1 is Latin America & Carribean and variable is BDH.new
#Clean the data
BDH.new # prints out values dataset2$BDH.new
NAs <- is.na(BDH.new) # records True values if the value is NA 
BDH.new.noNAs <- BDH.new[!NAs] # filters out NA values, new array
#NO NA's in BDH.new dataset

# Create subsets for first region
subset1 <- dataset2[dataset2$region == "Latin America & Caribbean", ]
head(subset1)

#1.1. Plot histograms for a variable of your choice for both regions with density lines overlayed.
#Cretae a histogram for Latin America & Carribean region with BDH.new variable

hist(subset1$BDH.new, seq(10., 90., 4.0), prob=TRUE)
lines (density(subset1$BDH.new,na.rm=TR,bw="SJ")) #SJ smooths out the density curve 
rug(subset1$BDH.new) #adds small tick marks along the x-axis, showing actual data points.

#1.2. Plot QQ plots for both variables compared to known probability distributions.
qqnorm(subset1$BDH.new); qqline(subset1$BDH.new)  #Q-Q Plot Against a Normal Distribution

qqplot(rnorm(250), subset1$BDH.new, xlab = "Q-Q plot for norm dsn") 
qqline(subset1$BDH.new) #Q-Q Plot Against a Standard Normal Distribution (Using Random Data)

install.packages("dplyr")

#2) Fit linear models

# Load necessary libraries
library(ggplot2)
library(dplyr)

#remove NA values
population.noNAs <-population[!NAs]
gdp.noNAs <- gdp[!NAs]

lmod <- lm(BDH.new ~ log10(population), data = dataset2)
## print model output
summary(lmod)
ggplot(dataset2, aes(x = log10(population), y = BDH.new)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

#plot residuals

residuals <- resid(lmod)  # Extract residuals
fitted_values <- fitted(lmod)  # Extract fitted values
plot(fitted_values, residuals, 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Adds a horizontal reference line

#2.2. Repeat the previous models with a subset of 1 region and in 1-2 sentences explain which model is a better fit and why you think that is the case.

str(subset1$population) #is population numeric ?
subset1$population <- as.numeric(as.character(subset1$population)) #if not convert to numeric

lmod <- lm(BDH.new ~ log10(population), data = subset1)
## print model output
summary(lmod)
ggplot(subset1, aes(x = log10(population), y = BDH.new)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

#The subset model is likely the better fit because it has higher R squared value and the p-value is significantly
#comparatively more significant Then the subset model would likely be a better fit, because:
#countries in one region might have more similar characteristics (e.g., economic policies, health systems).

#3.1 Train 2 kNN models using ”region” as the class label

subset3 <- dataset2[dataset2$region %in% c("Sub-Saharan Africa", "Global West"), 
                        c("ECO.old", "EPI.old", "BDH.old", "region")]

head(subset3) #see first couple columns of new subset

# Convert 'region' to a factor

install.packages("class")  # For kNN
install.packages("caret")  # For confusion matrix & accuracy calculation

library(class)
library(caret)

subset3$region <- as.factor(subset3$region)
str(subset3$region)

# Normalize the data using min-max normalization
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

# Apply normalization to the three chosen variables (ECO.old, EPI.old, BDH.old)
subset3[1:3] <- as.data.frame(lapply(subset3[1:3], normalize))

# Check summary of one normalized variable
summary(subset3$ECO.old)

# After normalization, each variable has a min of 0 and a max of 1.
# In other words, values are in the range from 0 to 1.

# Split the data into training (70%) and testing (30%) sets
set.seed(123)  # Ensures reproducibility
ind <- sample(2, nrow(subset3), replace = TRUE, prob = c(0.7, 0.3))
KNNtrain <- subset3[ind == 1, ]
KNNtest <- subset3[ind == 2, ]

# Check square root of sample size (useful for choosing k)
sqrt(nrow(KNNtrain)) #based on output use k =7 
help("knn")  # Read the kNN documentation on RStudio

# Train kNN model using the three selected features
KNNpred <- knn(train = KNNtrain[, 1:3],  # ECO.old, EPI.old, BDH.old
               test = KNNtest[, 1:3],    # ECO.old, EPI.old, BDH.old
               cl = KNNtrain$region,     # Target variable (Region)
               k = 7)                    # Using k = 7 (adjust if needed)

# Print predictions
KNNpred

# Create a table of predicted classes
table(KNNpred)

# Generate confusion matrix
conf_matrix <- confusionMatrix(as.factor(KNNpred), as.factor(KNNtest$region))
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(KNNpred == KNNtest$region) / length(KNNtest$region)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))


#################################################

# Create subsets for second region
subset2 <- dataset2[dataset2$region == "Global West", ]
head(subset2)

#2.1. Plot histograms for a variable of your choice for both regions with density lines overlayed.
#Create a histogram for the Global West region with BDH.new variable

hist(subset2$BDH.new, seq(30., 90., 5.0), prob=TRUE)
lines (density(subset2$BDH.new,na.rm=TR,bw="SJ")) #SJ smooths out the density curve 
rug(subset2$BDH.new) #adds small tick marks along the x-axis, showing actual data points.

#2.2. Plot QQ plots for both variables compared to known probability distributions.
qqnorm(subset2$BDH.new); qqline(subset2$BDH.new)  #Q-Q Plot Against a Normal Distribution

qqplot(rnorm(250), subset2$BDH.new, xlab = "Q-Q plot for norm dsn") 
qqline(subset2$BDH.new) #Q-Q Plot Against a Standard Normal Distribution (Using Random Data)

install.packages("dplyr")

#2.1.1) Fit linear models

# Load necessary libraries
library(ggplot2)
library(dplyr)

#remove NA values
population.noNAs <-population[!NAs]
gdp.noNAs <- gdp[!NAs]

lmod <- lm(BDH.old ~ log10(population), data = dataset2)
## print model output
summary(lmod)
ggplot(dataset2, aes(x = log10(population), y = BDH.old)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

#plot residuals

residuals <- resid(lmod)  # Extract residuals
fitted_values <- fitted(lmod)  # Extract fitted values
plot(fitted_values, residuals, 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Adds a horizontal reference line

#2.2.2 Repeat the previous models with a subset of 1 region and in 1-2 sentences explain which model is a better fit and why you think that is the case.

str(subset2$population) #is population numeric ?
subset2$population <- as.numeric(as.character(subset2$population)) #if not convert to numeric

lmod <- lm(BDH.old ~ log10(population), data = subset2)
## print model output
summary(lmod)
ggplot(subset2, aes(x = log10(population), y = BDH.old)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


#Neither dataset gives strong results, but dataset2 is slightly better because it at least has a significant intercept and a weak trend.
#Subset2 has almost no explanatory power, likely due to a small sample or lack of variability in BDH.old.


#3.1.1 Train 2nd kNN models using ”region” as the class label

subset4 <- dataset2[dataset2$region %in% c("Greater Middle East", "Southern Asia"), 
                    c("ECO.new", "EPI.new", "BDH.new", "region")]

head(subset4) #see first couple columns of new subset

# Convert 'region' to a factor

install.packages("class")  # For kNN
install.packages("caret")  # For confusion matrix & accuracy calculation

library(class)
library(caret)

subset4$region <- as.factor(subset4$region)
str(subset4$region)

# Normalize the data using min-max normalization
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

# Apply normalization to the three chosen variables (ECO.new, EPI.new, BDH.new)
subset4[1:3] <- as.data.frame(lapply(subset4[1:3], normalize))

# Check summary of one normalized variable
summary(subset4$ECO.new)

# After normalization, each variable has a min of 0 and a max of 1.
# In other words, values are in the range from 0 to 1.

# Split the data into training (70%) and testing (30%) sets
set.seed(123)  # Ensures reproducibility
ind <- sample(2, nrow(subset4), replace = TRUE, prob = c(0.7, 0.3))
KNNtrain <- subset4[ind == 1, ]
KNNtest <- subset4[ind == 2, ]

# Check square root of sample size (useful for choosing k)
sqrt(nrow(KNNtrain)) #based on output use k =4 
help("knn")  # Read the kNN documentation on RStudio

# Train kNN model using the three selected features
KNNpred <- knn(train = KNNtrain[, 1:3],  # ECO.new, EPI.new, BDH.new
               test = KNNtest[, 1:3],    # ECO.new, EPI.new, BDH.new
               cl = KNNtrain$region,     # Target variable (Region)
               k = 4)                    # Using k = 4 (adjust if needed)

# Print predictions
KNNpred

# Create a table of predicted classes
table(KNNpred)

# Generate confusion matrix
conf_matrix <- confusionMatrix(as.factor(KNNpred), as.factor(KNNtest$region))
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(KNNpred == KNNtest$region) / length(KNNtest$region)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

