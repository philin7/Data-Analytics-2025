library("ggplot2")
library("readr")
library(readxl)

head(Refinedemissions)
head(Refinedtourism)

nrow(Refinedemissions)
nrow(Refinedtourism)

#merge dataset by country and year

MergedData <- merge(Refinedemissions, Refinedtourism, by = c("Country", "Year"))

head(MergedData)

nrow(MergedData)

# performs a multiple linear regression to check how Country and Year predict CO2 emissions

lmod_co2 <- lm(`CO2 emissions (thousand metric tons of carbon dioxide)`~ Country + Year, data = MergedData)
summary(lmod_co2) # Check model performance

#performs a multiple linear regression to check how Country and Year predict tourism expenditure

lmod_tourism <- lm(`Tourism expenditure (millions of US dollars)` ~ Country + Year, data = MergedData)
summary(lmod_tourism)  # Check model performance

library(ggplot2)
library(tidyr)
library(dplyr)


# Rename columns to remove spaces and special characters
MergedData <- MergedData %>%
  rename(
    CO2_emissions = `CO2 emissions (thousand metric tons of carbon dioxide)`,
    Tourism_expend = `Tourism expenditure (millions of US dollars)`
  )

library(ggplot2)
library(dplyr)
library(tidyr)

# Filter dataset for selected countries(most visited countries in each continent)
selected_countries <- c("France", "United States of America", "Thailand", "South Africa", "Australia","Argentina")

MergedData_selected <- MergedData %>%
  filter(Country %in% selected_countries)

# Transform dataset into long format
MergedData_long_selected <- MergedData_selected %>%
  pivot_longer(cols = c(CO2_emissions, Tourism_expend), 
               names_to = "Variable", 
               values_to = "Value")

# Plot for selected countries with log scale
ggplot(MergedData_long_selected, aes(x = Year, y = Value, color = Variable, group = interaction(Country, Variable))) +
  geom_line(size = 1) +  # Line plot to show trends over time
  geom_point(alpha = 0.7) +  # Points for emphasis
  scale_y_log10() +  # Apply log transformation to the y-axis
  facet_wrap(~Country, scales = "free_y") +  # Separate plots for each country
  labs(title = "CO2 Emissions and Tourism Expenditure (Log Scale)",
       x = "Year",
       y = "Log(Value)",
       color = "Variable") +  # Differentiating CO2 vs Tourism Expenditure
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

####################
#Visualize countries with share of tourism contributing to GDP above 10%, which signals that tourism plays a significant role in country's overall GDP

colnames(X_of_tourism_in_gdp_by_country)

# Check if dataset exists and has correct columns
colnames(X_of_tourism_in_gdp_by_country)

# Filter dataset for GDP from tourism above 10%
FilteredGDPData <- X_of_tourism_in_gdp_by_country %>%
  filter(`GDP from tourism as a share of total GDP` > 10)

# Create the plot
ggplot(FilteredGDPData, aes(x = `GDP from tourism as a share of total GDP`, 
                            y = reorder(Country, `GDP from tourism as a share of total GDP`), 
                            fill = Country)) +
  geom_col() +
  scale_x_continuous(limits = c(0, max(FilteredGDPData$`GDP from tourism as a share of total GDP`, na.rm = TRUE))) +
  labs(title = "Countries with GDP from Tourism Above 10%",
       x = "GDP from Tourism as % of Total GDP",
       y = "Country") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend if too many colors


#####

#see if there is a correlation between tourism expenditure and CO2 emissions in tourism reliant countries

# Define high-tourism-GDP countries
high_tourism_countries <- c("Macao", "St. Lucia", "Palau", "Aruba", "Bahamas", 
                            "Guam", "Fiji", "Philippines", "Croatia", "Antigua and Barbuda")

# Scatter plot with log scale for better visualization
ggplot(FilteredData, aes(x = Tourism_expend, y = CO2_emissions, color = Country)) +
  geom_point(size = 3, alpha = 0.7) +  # Data points
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  # Trend line
  scale_x_log10() +  # Apply log scale to X-axis (Tourism expenditure)
  scale_y_log10() +  # Apply log scale to Y-axis (CO2 emissions)
  labs(title = "Tourism Expenditure vs CO₂ Emissions (Log Scale) in High-Tourism GDP Countries",
       x = "Tourism Expenditure (Log Scale, Million USD)",
       y = "CO₂ Emissions (Log Scale, Thousand Metric Tons)",
       color = "Country") +
  theme_minimal()

#check how well model performed

# Filter the dataset for high-tourism-GDP countries
FilteredData_high_tourism <- FilteredData %>%
  filter(Country %in% high_tourism_countries)

# Fit the linear regression model for high-tourism countries
model_high_tourism <- lm(log(CO2_emissions) ~ log(Tourism_expend), data = FilteredData_high_tourism)

# Display the summary of the model to get R-squared value
summary(model_high_tourism)

##############################################################

#Re-run model without Philippines as it is a major outlier 

# Define high-tourism-GDP countries (excluding Philippines)
high_tourism_countries_no_philippines <- c("Macao", "St. Lucia", "Palau", "Aruba", "Bahamas", 
                                           "Guam", "Fiji", "Croatia", "Antigua and Barbuda")

# Filter the dataset
FilteredData_no_philippines <- FilteredData %>%
  filter(Country %in% high_tourism_countries_no_philippines)

# Refit the model without the Philippines
model_no_philippines <- lm(log(CO2_emissions) ~ log(Tourism_expend), data = FilteredData_no_philippines)

# Display the updated R-squared value
summary(model_no_philippines)

# Scatter plot without Philippines
ggplot(FilteredData_no_philippines, aes(x = Tourism_expend, y = CO2_emissions, color = Country)) +
  geom_point(size = 3, alpha = 0.7) +  # Plot points
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  # Regression line
  scale_x_log10() +  # Log scale for Tourism Expenditure
  scale_y_log10() +  # Log scale for CO2 Emissions
  labs(title = "Tourism Expenditure vs CO₂ Emissions (Log Scale) Without the Philippines",
       x = "Tourism Expenditure (Log Scale, Million USD)",
       y = "CO₂ Emissions (Log Scale, Thousand Metric Tons)",
       color = "Country") +
  theme_minimal()

##################################


#look at relationship between CO2 emissions and tourism expenditure in most visited countries for each continent

# Filter dataset for selected countries from MergedData
FilteredData_selected <- MergedData %>%
  filter(Country %in% selected_countries)

# Scatter plot with log scale for better visualization
ggplot(FilteredData_selected, aes(x = Tourism_expend, y = CO2_emissions, color = Country)) +
  geom_point(size = 3, alpha = 0.7) +  # Data points
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  # Trend line
  scale_x_log10() +  # Apply log scale to X-axis (Tourism expenditure)
  scale_y_log10() +  # Apply log scale to Y-axis (CO2 emissions)
  labs(title = "Tourism Expenditure vs CO₂ Emissions (Log Scale) in Selected Countries",
       x = "Tourism Expenditure (Log Scale, Million USD)",
       y = "CO₂ Emissions (Log Scale, Thousand Metric Tons)",
       color = "Country") +
  theme_minimal()

# Fit a linear model
model <- lm(log(CO2_emissions) ~ log(Tourism_expend), data = FilteredData_selected)

# Print model summary
summary(model)

#######################################

head(MergedData)

# Merge X_of_tourism_in_gdp_by_country into MergedData by Country and Year
TriMerge <- merge(MergedData, X_of_tourism_in_gdp_by_country, by = c("Country", "Year"))

head(TriMerge)

# Load libraries
library(dplyr)
library(ggplot2)
install.packages("factoextra")
library(factoextra)


# Step 1: Select relevant numeric variables and remove NAs
pca_data <- TriMerge %>%
  select(Country, `CO2_emissions`, `Tourism_expend`, `GDP from tourism as a share of total GDP`) %>%
  na.omit()

# Step 2: Perform PCA on the numeric columns (excluding 'Country')
pca_result <- prcomp(pca_data[, -1], scale. = TRUE)

# Step 3: Extract PCA scores (coordinates in new space)
pca_scores <- as.data.frame(pca_result$x)
pca_scores$Country <- pca_data$Country  # add back country names

# Step 4: K-means clustering (choose k = 3, adjust as needed)
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(pca_scores[, 1:2], centers = 3)  # using first 2 principal components

# Step 5: Add cluster assignments
pca_scores$Cluster <- factor(kmeans_result$cluster)

# Reattach the Country column (make sure row order hasn’t changed)
pca_scores$Country <- TriMerge$Country


# Step 6: Visualize PCA with clusters
ggplot(pca_scores, aes(x = PC1, y = PC2, color = Cluster, label = Country)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(size = 2.5, vjust = 1.5, hjust = 0.5) +
  labs(title = "PCA + K-means Clustering of Countries",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme_minimal()

# Assuming 'pca_scores' contains the columns 'Country' and 'Cluster'
install.packages("writexl")
library(writexl)

# Export to Excel
write_xlsx(pca_scores[, c("Country", "Cluster")], "full_country_cluster_mapping.xlsx")

getwd()

# Trying to predict or explain changes in CO₂ emissions based on how much countries spend on tourism, how reliant they are on tourism for GDP, and what year the data is from.
#Log-transformed CO₂ emissions and tourism expenditure to normalize the data and interpret relationships in terms of percent changes

# Log-transformed multiple regression
log_model <- lm(log(CO2_emissions) ~ log(Tourism_expend) + `GDP from tourism as a share of total GDP` + Year, data = TriMerge)

# View the summary
summary(log_model)

#####################

#Time series analysis for USA

usa_data <- MergedData %>%
  filter(Country == "United States of America")

usa_long <- usa_data %>%
  pivot_longer(cols = c(CO2_emissions, Tourism_expend),
               names_to = "Variable",
               values_to = "Value")

library(ggplot2)

ggplot(usa_long, aes(x = Year, y = Value, color = Variable)) +
  geom_line(size = 1.2) +
  geom_point(alpha = 0.7) +
  scale_y_log10() +  # Apply log scale to the y-axis
  labs(title = "Tourism Expenditure and CO₂ Emissions Over Time (U.S.) - Log Scale",
       x = "Year",
       y = "Log-scaled Value",
       color = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############################################

#comparing the distribution of a C02 emissions across high tourism countries

# Define the high-tourism countries
high_tourism_country_list <- c("Macao", "St. Lucia", "Palau", "Aruba", "Bahamas", 
                               "Guam", "Fiji", "Croatia","Philippines", "Antigua and Barbuda")

# Filter the MergedData dataset
high_tourism_countries <- MergedData %>%
  filter(Country %in% high_tourism_country_list)


ggplot(high_tourism_countries, aes(x = Country, y = CO2_emissions)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", outlier.shape = 1) +
  labs(title = "Distribution of CO₂ Emissions by Country",
       x = "Country",
       y = "CO₂ Emissions (thousand metric tons)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#now distribution of tourism expenditure

ggplot(high_tourism_countries, aes(x = Country, y = Tourism_expend)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", outlier.shape = 1) +
  labs(title = "Distribution of Tourism Expenditure by Country",
       x = "Country",
       y = "Tourism Expenditure (millions of US dollars)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#comparing the distribution of CO2 emissions across most visited countries in each Continent

# Define the most visited countries from each continent
most_visited_countries <- c("United States of America", "Australia", "Argentina", 
                            "South Africa", "France", "Thailand")

# Filter the MergedData dataset
most_visited_data <- MergedData %>%
  filter(Country %in% most_visited_countries)

# Boxplot of CO2 emissions for most visited countries
ggplot(most_visited_data, aes(x = Country, y = CO2_emissions, fill = Country)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution of CO₂ Emissions in Most Visited Countries",
       x = "Country",
       y = "CO₂ Emissions (Thousand Metric Tons)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

#now for tourism expenditure

ggplot(most_visited_data, aes(x = Country, y = Tourism_expend, fill = Country)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution of Tourism Expenditure in Most Visited Countries",
       x = "Country",
       y = "Tourism Expenditure (Millions of US dollars)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

##################################################################


library(class)
library(dplyr)

# Step 1: Select relevant numeric columns and remove NAs
knn_data <- na.omit(MergedData[, c("Tourism_expend", "CO2_emissions", "Country")])

# Step 2: Normalize only the numeric features
knn_features <- scale(knn_data[, c("Tourism_expend", "CO2_emissions")])
knn_scaled <- as.data.frame(knn_features)

# Step 3: Add back the target variable (Country)
knn_scaled$Country <- knn_data$Country

# Step 4: Train/test split
set.seed(123)
train_idx <- sample(1:nrow(knn_scaled), 0.7 * nrow(knn_scaled))
train <- knn_scaled[train_idx, ]
test <- knn_scaled[-train_idx, ]

# Step 5: Run KNN classification
pred <- knn(train[, 1:2], test[, 1:2], cl = train$Country, k = 3)

# Step 6: Check accuracy
accuracy <- mean(pred == test$Country)
accuracy


# Load necessary package
library(caret)

# Ensure 'pred' and 'test$Country' are factors
pred <- factor(pred)
actual <- factor(test$Country)

# Align the levels of both factors
common_levels <- union(levels(pred), levels(actual))
pred <- factor(pred, levels = common_levels)
actual <- factor(actual, levels = common_levels)

# Generate the confusion matrix
confusionMatrix(pred, actual)


################################################


# Step 1: Select relevant numeric columns and remove NAs
knn_data <- na.omit(MergedData[, c("Tourism_expend", "CO2_emissions", "Country")])

# Step 2: Filter for 30 randomly selected countries
selected_countries <- c(
  "Argentina", "Australia", "Austria", "Belgium", "Brazil", "Canada", "Chile", "China",
  "Colombia", "Czech Republic", "Denmark", "Egypt", "Finland", "France", "Germany", "Greece",
  "Hungary", "India", "Indonesia", "Ireland", "Israel", "Italy", "Japan", "Mexico",
  "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Russia"
)

knn_data <- knn_data %>% filter(Country %in% selected_countries)

# Step 3: Normalize only the numeric features
knn_features <- scale(knn_data[, c("Tourism_expend", "CO2_emissions")])
knn_scaled <- as.data.frame(knn_features)

# Step 4: Add back the target variable (Country)
knn_scaled$Country <- knn_data$Country

# Step 5: Train/test split
set.seed(123)
train_idx <- sample(1:nrow(knn_scaled), 0.7 * nrow(knn_scaled))
train <- knn_scaled[train_idx, ]
test <- knn_scaled[-train_idx, ]

# Step 6: Run KNN classification
pred <- knn(train[, 1:2], test[, 1:2], cl = train$Country, k = 3)

# Step 7: Check accuracy
accuracy <- mean(pred == test$Country)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

######################

#High Number of Classes: With 30 countries, random guessing would yield about a 3.33% accuracy. Your model's 13.24% accuracy is significantly better than random, indicating it captures some patterns.​

#Limited Features: Using only two features may not provide enough information to distinguish between countries with similar CO₂ emissions and tourism expenditure.​

#Feature Overlap: Countries may have overlapping values in these features, making it challenging for the model to differentiate between them.

###################################



