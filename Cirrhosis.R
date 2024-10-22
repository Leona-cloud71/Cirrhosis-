
library(readr)
library(ggplot2)     # For visualization
library(dplyr)       # For data manipulation
library(tidyr)       # For data tidying
library(caret)       # For machine learning functions (train, confusionMatrix, etc.)
library(xgboost)     # XGBoost implementation in R
library(catboost)    # CatBoost implementation in R
library(randomForest) # RandomForest implementation in R
library(e1071)       # For confusion matrix and other evaluation metrics
library(mltools)     # For additional metrics
library(gridExtra)
library(reshape2)
library(ggcorrplot)

set.seed(123)


Cirrhosis<- read_csv("C:/Users/Lib 003/Desktop/Kaggle Website Research/CIRRHOSIS/CIrrhosis Dataset.csv")


Cirrhosis <- Cirrhosis %>% select(-ID)

head(Cirrhosis)

nrow(Cirrhosis)

# Remove rows with any missing values
Cirrhosis_1<- na.omit(Cirrhosis)

nrow(Cirrhosis_1)

# Display the cleaned data frame
head(Cirrhosis_1)

summary(Cirrhosis_1)

####### summarystatistics for clean data

Cirrhosis_1 <- Cirrhosis_1 %>%
  select_if(is.numeric) %>%
  summary()
Cirrhosis_1






# Count the frequency of each Status
Cases <- table(Cirrhosis$Status)

# Define colors
colors <- c("red", "blue", "green")

# Create a two-plot layout using par
par(mfrow = c(1, 2))  # Set up a 1x2 layout

# Bar chart on the first plot
barplot(Cases, 
        col = colors, 
        xlab = 'Status', 
        ylab = 'Frequency', 
        main = 'Distribution of Status', 
        border = 'white')


# Calculate percentages
percentages <- round(100 * prop.table(Cases), 1)

# Create labels for the pie chart with percentages
labels_with_percent <- paste(names(Cases), "\n", percentages, "%", sep = "")

# Pie chart on the second plot
pie(Cases, 
    labels = labels_with_percent, 
    col = colors, 
    explode = rep(0.05, length(Cases)), 
    main = 'Distribution of Status')




# Specify columns to plot
columns_to_plot <- c('N_Days', 'Age', 'Bilirubin', 'Cholesterol', 
                     'Albumin', 'Copper', 'Alk_Phos', 'SGOT', 
                     'Tryglicerides', 'Platelets', 'Prothrombin', 'Stage')

# Check if specified columns exist in the DataFrame
existing_columns <- columns_to_plot[columns_to_plot %in% colnames(Cirrhosis)]
print(existing_columns)  # Print existing columns to check

# Create a list to hold plots
plot_list <- list()

# Loop through each column and create histograms with KDE
for (column in existing_columns) {
  p <- ggplot(Cirrhosis, aes_string(x = column)) +
    geom_histogram(aes(y = ..density..), bins = 20, fill = "brown", color = "black", alpha = 0.7) +
    geom_density(color = "blue", linewidth = 1) +  # Changed 'size' to 'linewidth'
    labs(x = column, y = "Density", title = paste("Distribution of", column)) +
    theme_minimal()
  
  plot_list[[column]] <- p  # Store the plot in the list
}

# Split plots into two sets of 10
plots_set1 <- plot_list[1:6]
plots_set2 <- plot_list[7:12]


# Calculate the number of rows and columns for the grid
n_cols <- 2
n_rows <- ceiling(length(plots_set1) / n_cols)

# Arrange and display the first set of plots in a grid layout
do.call(grid.arrange, c(plots_set1, ncol = n_cols))

# Arrange and display the second set of plots in a grid layout
do.call(grid.arrange, c(plots_set2, ncol = n_cols))


#############categorical Features Visualization

# Define a purple color palette
purple_palette <- c("#800080", "#9B59B6", "#A020F0", "#6A5ACD", "#9370DB")

# Specify columns to plot
columns_to_plot <- c("Drug", "Sex", "Ascites", "Hepatomegaly", "Spiders", "Edema")

# Set up layout for multiple plots
nrows <- length(columns_to_plot)
####par(mfrow = c(nrows, 2))  # Two columns for bar plots and pie charts

# Loop through each column and create bar plots and pie charts
for (column in columns_to_plot) {
  # Create bar plot
  bar_plot <- ggplot(Cirrhosis, aes_string(x = column, fill = column)) +
    geom_bar() +
    scale_fill_manual(values = purple_palette) +  # Use the purple palette
    labs(x = column, y = "Count", title = paste("Distribution of", column)) +
    theme_minimal() +
    geom_text(stat = 'count', aes(label = ..count..), 
              position = position_stack(vjust = 0.5), color = "white", size = 4)  # Add counts
  
  print(bar_plot)  # Display the bar plot
  
  # Create pie chart
  values <- Cirrhosis[[column]] %>% table() %>% sort(decreasing = TRUE)
  
  pie(values, 
      labels = paste(names(values), "\n", round(100 * values / sum(values), 1), "%"), 
      col = purple_palette[1:length(values)],  # Use the purple palette
      main = paste("Distribution of", column),
      cex = 0.8)  # Adjust text size
}



############Distribution of features vs. Target

library(ggplot2)

# Create the count plot
ggplot(Cirrhosis_1, aes(x = Drug, fill = as.factor(Stage))) + 
  geom_bar(position = "dodge") + 
  labs(title = "Drug Distribution by Stage", x = "Drug", y = "Count") +
  theme_minimal()


# Specify columns to plot
columns_to_plot <- c("Drug", "Sex", "Ascites", "Hepatomegaly", "Spiders", "Edema")

# Define the color palette
palette <- c("brown", "grey", "blue")

# Loop through each column and create count plots
for (col in columns_to_plot) {
  # Create the count plot
  count_plot <- ggplot(Cirrhosis, aes_string(x = col, fill = "Status")) +
    geom_bar(position = "dodge") +  # Use position dodge for side-by-side bars
    scale_fill_manual(values = palette) +  # Apply the custom color palette
    labs(title = paste("Distribution of", col, "by Target Variable"), x = col, y = "Count") +
    theme_minimal()  # Use a minimal theme
  
  # Display the plot
  print(count_plot)
}


######Correlation Matrix


# Assuming df is your DataFrame, first ensure to select only numeric columns
numeric_columns <- sapply(Cirrhosis, is.numeric)  # Identify numeric columns
cor_matrix <- cor(Cirrhosis[, numeric_columns], use = "complete.obs")  # Calculate correlation for numeric columns

# Melt the correlation matrix into long format
cor_melted <- melt(cor_matrix)

# Create a heatmap using ggplot2
ggplot(data = cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Add tiles with white borders
  scale_fill_gradient(low = "green", high = "yellow") +  # Use blue gradient for color
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Add correlation coefficients as text
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels




######## Encoding the stutus, Drug,Sex,Ascites,Hepatomegaly,Spiders,Edema,Stage columns


library(dplyr)

# Check the structure of the dataset to identify categorical columns
str(Cirrhosis_1)

# Step 1: Encoding the Status column with custom mapping

target_mapping <- c('D' = 0, 'C' = 1, 'CL' = 2) 
Cirrhosis_1$Status <- as.numeric(recode(Cirrhosis_1$Status, !!!target_mapping))

# Step 2: Identifying categorical columns
# List the categorical columns that need to be encoded

categorical_cols <- c('Drug', 'Sex', 'Ascites', 'Hepatomegaly', 'Spiders', 'Edema')

# Step 3: Encoding the categorical columns

for (col in categorical_cols) {
  Cirrhosis_1[[col]] <- as.numeric(as.factor(Cirrhosis_1[[col]]))
}

# Step 4: Verify the transformation

str(Cirrhosis_1)

head(Cirrhosis_1)

############################################## Model Preparation

# Check for missing values
sum(is.na(Cirrhosis_1))  # Total number of missing values
colSums(is.na(Cirrhosis_1))  # Missing values by column



x <- Cirrhosis_1 %>% select(-Status)
y <- Cirrhosis_1$Status


head(x)
head(y)



# Partition the data
set.seed(42)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
x_train <- x[trainIndex, ]
x_test <- x[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Train the Random Forest model
set.seed(42)
rf_model <- randomForest(x = x_train, y = as.factor(y_train), ntree = 500)

# Make predictions
rf_predictions <- predict(rf_model, newdata = x_test)

# Evaluate the model
conf_matrix <- confusionMatrix(as.factor(rf_predictions), as.factor(y_test))
print(conf_matrix)



# Step 4: Feature Importance (Optional but useful for analysis)
importance(rf_model)  
 
varImpPlot(rf_model, main = "Random Forest - Feature Importance")


#############################################

# Extract the feature importance
importance_scores <- importance(rf_model)

# Convert to a data frame for plotting
importance_df <- data.frame(
  Feature = rownames(importance_scores),
  Importance = importance_scores[, "MeanDecreaseGini"]
)

# Sort the features by importance
importance_df <- importance_df %>%
  arrange(desc(Importance))

# Plot the feature importance as a bar chart
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +  # Flip the coordinates to have a horizontal bar chart
  labs(title = "Feature Importance from Random Forest",
       x = "Features",
       y = "Importance") +
  theme_minimal()



