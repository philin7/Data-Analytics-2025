head(ObesityDataSet_raw_and_data_sinthetic)

#rename dataset
ODS <- ObesityDataSet_raw_and_data_sinthetic

head(ODS)

#Exploratory Analysis 

# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)

#1. Handle missing values (check & optionally remove)
colSums(is.na(ODS))  # see how many NAs

# no NA's so no need to remove anything

#2. Descriptive statistics for numeric columns 
summary(select(ODS, Age, Height, Weight))

#3. Histograms for numerical columns

ggplot(ODS, aes(x = Age)) + geom_histogram(binwidth = 2, fill = "skyblue", color = "black") + theme_minimal() #most ages consolidated in the 20's (right skewed, little ages consolidated past the 40 age range)

ggplot(ODS, aes(x = Height)) + geom_histogram(binwidth = 0.02, fill = "salmon", color = "black") + theme_minimal() #follows normal distribution pretty well which is to be expected because it is height, majoirty of height clustered between 1.6 - 1.8 m 

ggplot(ODS, aes(x = Weight)) + geom_histogram(binwidth = 2, fill = "lightgreen", color = "black") + theme_minimal() 


#4. Boxplots for outlier detection
ggplot(ODS, aes(y = Weight)) + geom_boxplot(fill = "orange") + theme_minimal()
ggplot(ODS, aes(y = Height)) + geom_boxplot(fill = "purple") + theme_minimal()
ggplot(ODS, aes(y = Age)) + geom_boxplot(fill = "blue") + theme_minimal()


#6. Remove outliers using IQR method ---
  remove_outliers <- function(x) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
    H <- 1.5 * IQR(x, na.rm = TRUE)
    x[x >= (qnt[1] - H) & x <= (qnt[2] + H)]
  }

ODS_clean <- ODS %>%
  filter(Weight %in% remove_outliers(Weight),
         Height %in% remove_outliers(Height))

##########################################################

#2. Model Development, Validation and Optimization 

#1st Model random tree


# Load libraries
library(dplyr)
library(ggplot2)
library(caret)
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Ensure target is a factor
ODS_clean$NObeyesdad <- as.factor(ODS_clean$NObeyesdad)

# Split data into training and testing
set.seed(123)
split <- createDataPartition(ODS_clean$NObeyesdad, p = 0.7, list = FALSE)
train_cls <- ODS_clean[split, ]
test_cls <- ODS_clean[-split, ]

# Train decision tree model
tree_model <- rpart(NObeyesdad ~ Age + Height + Weight + FAF + FAVC + FCVC + CH2O + NCP, data = train_cls, method = "class")
rpart.plot(tree_model)


# Predict and evaluate
pred_cls <- predict(tree_model, test_cls, type = "class")
confusionMatrix(pred_cls, test_cls$NObeyesdad)



#2nd model, regression analysis to predict weight based on lifestyle and demographic features.

# Ensure necessary packages are loaded
library(ggplot2)

# Build the model
reg_model <- lm(Weight ~ Age + Height + FAF + CH2O + FCVC + NCP, data = ODS_clean)

# View summary stats
summary(reg_model)

# Plot actual vs predicted weight
ODS_clean$Predicted_Weight <- predict(reg_model, ODS_clean)

ggplot(ODS_clean, aes(x = Weight, y = Predicted_Weight)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Actual vs Predicted Weight",
       x = "Actual Weight",
       y = "Predicted Weight")

#3rd model, clustering w/ kmeans

# Scale selected numeric lifestyle variables
lifestyle_vars <- ODS_clean %>%
  select(FAF, TUE, FCVC, CH2O, NCP)

scaled_lifestyle <- scale(lifestyle_vars)

# Determine optimal number of clusters using Elbow Method
wss <- sapply(1:10, function(k){
  kmeans(scaled_lifestyle, k, nstart = 10)$tot.withinss
})
plot(1:10, wss, type = "b",
     main = "Elbow Method for Optimal K",
     xlab = "Number of Clusters",
     ylab = "Within-cluster Sum of Squares")

# Choose k = 3 for simplicity and good spread
set.seed(123)
kmeans_model <- kmeans(scaled_lifestyle, centers = 3, nstart = 25)

# Add cluster labels to original dataset
ODS_clean$Cluster <- as.factor(kmeans_model$cluster)

# Visualize clusters
ggplot(ODS_clean, aes(x = FAF, y = FCVC, color = Cluster)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "K-Means Clustering of Lifestyle Patterns",
       x = "Physical Activity Frequency (FAF)",
       y = "Vegetable Consumption (FCVC)")




