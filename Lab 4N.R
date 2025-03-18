

colnames(wine) <- c("class","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavinoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")
head(wine)

# Load necessary libraries
library(ggplot2)

# Perform PCA (center and scale the data)
wine_pca <- prcomp(wine, center = TRUE, scale. = TRUE)

# Extract the first two principal components
pc_data <- data.frame(PC1 = wine_pca$x[,1], PC2 = wine_pca$x[,2])

# Plot the first two principal components
ggplot(pc_data, aes(x = PC1, y = PC2)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "PCA Plot of Wine Dataset", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

# Identify the variables that contribute the most to the 1st PC.

# Extract loadings (eigenvectors)
pc1_loadings <- abs(wine_pca$rotation[,1])  # Take absolute values

# Sort variables by contribution to PC1
pc1_sorted <- sort(pc1_loadings, decreasing = TRUE)

# Display the top contributing variables
pc1_sorted

#Based on results, the variables contributing the most to the first principal component (PC1) are:
#Flavonoids (0.391)
#Total phenols (0.359)
#OD280/OD315 of diluted wines (0.351)
#Proanthocyanins (0.279)

#Identify the variables that contribute the least, drop them, then rerun PCA

# Remove the least contributing variables: "Ash" and "Color intensity"
wine_refined <- subset(wine, select = -c(`Ash`, `Color intensity`))  

# Perform PCA again on the refined dataset
wine_pca_refined <- prcomp(wine_refined, center = TRUE, scale. = TRUE)

# Extract updated PC1 loadings
pc1_loadings_refined <- abs(wine_pca_refined$rotation[,1])

# Sort variables by contribution to PC1
pc1_sorted_refined <- sort(pc1_loadings_refined, decreasing = TRUE)

# Display the top contributing variables after removing least contributing ones
pc1_sorted_refined

#Train a classifier model (e.g. kNN) to predict wine type using the original dataset.

# Convert the Class column to a factor
wine$class <- as.factor(wine$class)

# Verify the change
str(wine)  # Check the structure of the dataset

library(class)
library(caret)

# Ensure Class is a factor (if not already)
wine$class <- as.factor(wine$class)

# Split dataset into 80% training and 20% testing (No Scaling)
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(wine), 0.8 * nrow(wine))  
train_data <- wine[train_index, ]
test_data <- wine[-train_index, ]

# Run kNN model (k=5)
k <- 5
knn_predictions <- knn(train = train_data[,-1], 
                       test = test_data[,-1], 
                       cl = train_data$class, 
                       k = k)
head(knn_predictions)

table(knn_predictions)

# Analyze accuracy of model

# Create confusion matrix
conf_matrix <- confusionMatrix(knn_predictions, test_data$class)

# Print confusion matrix
print(conf_matrix$table)  # Contingency table
print(conf_matrix_knn)


#Class 1 & 2 are well-classified, but Class 3 absorbs many mis-classifications.
#Class 2 has high mis-classification (8 times as Class 3).

#Train second kNN model to predict wine type using the data projected into the first 3 PCs

# Extract first 3 principal components
wine_pca_scores <- data.frame(wine_pca$x[, 1:3])  # Keep PC1, PC2, PC3
wine_pca_scores$class <- wine$class  # Add Class column back

set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(wine_pca_scores), 0.8 * nrow(wine_pca_scores))  

train_data <- wine_pca_scores[train_index, ]
test_data <- wine_pca_scores[-train_index, ]

# Set number of neighbors (k)
k <- 5  # Adjust k as needed

# Train kNN model
knn_predictions <- knn(train = train_data[,-4],  # Exclude class column
                       test = test_data[,-4], 
                       cl = train_data$class, 
                       k = k)
head(knn_predictions)
table(knn_predictions)

summary(wine_pca)  # Check % variance explained by each PC (PC1, PC2, and PC3 explain most of the variance which is why models may perform similarly, also same split (80/20))

# Compute confusion matrix
conf_matrix_knn <- confusionMatrix(knn_predictions, test_data$class)
print(conf_matrix_knn)

#The model is fairly accurate (72%), but Class 2 is often misclassified as Class 3.
#Recall for Class 2 is low (44%), meaning the model struggles to identify it.
#Precision for Class 3 is low (50%), meaning there are many false positives for Class 3.
#Kappa Score = 0.6004 → Indicates moderate agreement beyond random chance.
#McNemar's Test (p = 0.0186) → Suggests the model may be biased towards misclassifying certain classes.