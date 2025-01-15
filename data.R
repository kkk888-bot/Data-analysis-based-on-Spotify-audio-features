#1. Data loading and preprocessing
library(ggplot2)
library(dplyr)
spotify_data<-read.csv("dataset.csv")
str(spotify_data)
summary(spotify_data)
spotify_data$artists <- strsplit(as.character(spotify_data$artists), ";")

#2. Exploratory data analysis (EDA)
install.packages("ggplot2")
install.packages("dplyr")
install.packages("caret")  
library(ggplot2)
library(dplyr)
library(caret)
# Genre and danceability distribution Box plot of the danceability of each genre:
ggplot(spotify_data, aes(x = track_genre, y = danceability, fill = track_genre)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Danceability Across Music Genres",
    x = "Genre",
    y = "Danceability"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Popularity distribution within music genres
ggplot(spotify_data, aes(x = track_genre, y = popularity, fill = track_genre)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Popularity Distribution Across Music Genres",
    x = "Genre",
    y = "Popularity"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Loudness vs. energy: Plot loudness vs. energy:
ggplot(spotify_data, aes(x = loudness, y = energy, color = track_genre)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Loudness vs Energy by Genre",
    x = "Loudness (dB)",
    y = "Energy",
    color = "Genre"
  )
# Draw a histogram of danceability
ggplot(spotify_data, aes(x = danceability)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "blue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Danceability", x = "Danceability", y = "Density") +theme_minimal()
# Draw a histogram of valence
ggplot(spotify_data, aes(x = valence)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "green", alpha = 0.7) +
  geom_density(color = "red", size = 1) +labs(title = "Distribution of Valence", x = "Valence", y = "Density") +theme_minimal()
# Draw a histogram of energy
ggplot(spotify_data, aes(x = energy)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "blue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Distribution of Energy",
    x = "Energy",
    y = "Density"
  ) +
  theme_minimal()
# Plot the scatter diagram of energy and loudness.
ggplot(spotify_data, aes(x = loudness, y = energy)) +
  geom_point(alpha = 0.7, color = "blue") +
  labs(title = "Scatterplot of Loudness vs Energy", x = "Loudness", y = "Energy") +theme_minimal()
# Plot valence and danceability as a scatter plot
ggplot(spotify_data, aes(x = danceability, y = valence)) +
  geom_point(alpha = 0.7, color = "green") +
  labs(title = "Scatterplot of Danceability vs Valence", x = "Danceability", y = "Valence") +theme_minimal()

#3. Divide the training set and the test set
# Split the dataset: 80% for the training set and 20% for the test set
set.seed(42)  
train_index <- createDataPartition(dataset_clean$track_genre, p = 0.8, list = FALSE)
train_data <- dataset_clean[train_index, ]
test_data <- dataset_clean[-train_index, ]

#4. Train a random forest model
set.seed(42)
rf_model <- randomForest(
  track_genre ~ .,  
  data = train_data,
  ntree = 100,      
  mtry = 3,         
  importance = TRUE 
)
print(rf_model)
plot(rf_model)

#5. Evaluate model performance
predictions <- predict(rf_model, newdata = test_data)
head(predictions)
conf_matrix <- confusionMatrix(predictions, test_data$track_genre)
print(conf_matrix)
accuracy <- conf_matrix$overall["Accuracy"]
print(paste("Model Accuracy:", accuracy))


#6. Visual confusion matrix
conf_matrix_data <- as.data.frame.table(conf_matrix$table)
# Plot confusion matrix heatmap
ggplot(conf_matrix_data, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    title = "Confusion Matrix Heatmap",
    x = "Predicted Genre",
    y = "Actual Genre"
  ) +
  theme_minimal()
# Calculate feature importance
importance_values <- importance(rf_model)
importance_df <- data.frame(
  Feature = rownames(importance_values), 
  Importance = importance_values[, "MeanDecreaseGini"] 
)
importance_df$Importance <- importance_df$Importance / sum(importance_df$Importance)
print(importance_df)

#7. Use random forests to classify the dataset you provide, including data pre-processing, model training and evaluation.
install.packages("randomForest")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)
dataset <- read.csv("dataset.csv")
str(dataset)
summary(dataset)
dataset$track_genre <- as.factor(dataset$track_genre)
dataset_clean <- dataset %>%
  select(-track_id, -track_name, -album_name, -artists)
dataset_clean <- na.omit(dataset_clean)

#8. Logistic regression
library(caret)
library(tidyverse)
data <- read.csv("dataset.csv")
data_selected <- data %>%
  select(track_genre, loudness, danceability, key)
data_selected$track_genre <- as.factor(data_selected$track_genre)
set.seed(123) 
train_index <- createDataPartition(data_selected$track_genre, p = 0.8, list = FALSE)
train_data <- data_selected[train_index, ]
test_data <- data_selected[-train_index, ]
logistic_model <- glm(track_genre ~ loudness + danceability + key, 
                      data = train_data, 
                      family = binomial)
summary(logistic_model)
test_data$predicted <- predict(logistic_model, newdata = test_data, type = "response")
test_data$predicted_class <- ifelse(test_data$predicted > 0.5, 1, 0) 
conf_matrix <- confusionMatrix(as.factor(test_data$predicted_class), 
                               as.factor(ifelse(test_data$track_genre == levels(test_data$track_genre)[1], 0, 1)))
print(conf_matrix)
importance <- abs(coef(logistic_model)[-1]) 
names(importance) <- c("loudness", "danceability", "key")
importance <- importance / sum(importance) 
importance_df <- data.frame(Feature = names(importance), Importance = importance)
importance_df <- importance_df[order(-importance_df$Importance), ]
print(importance_df)

library(ggplot2)
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance (Logistic Regression)",
       x = "Features", 
       y = "Importance") +
  theme_minimal()

#9.
library(caret)
y_true <- test_data$track_genre
y_pred <- predict(rf_model, test_data)
conf_matrix <- confusionMatrix(y_pred, y_true)
accuracy <- conf_matrix$overall['Accuracy'] 
class_metrics <- conf_matrix$byClass 
precision <- mean(class_metrics[, "Precision"], na.rm = TRUE)
recall <- mean(class_metrics[, "Recall"], na.rm = TRUE)
f1_score <- mean(class_metrics[, "F1"], na.rm = TRUE)
performance_table <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1-Score"),
  Value = c(accuracy, precision, recall, f1_score)
)
print(performance_table)


#10. Load random forest library
library(randomForest)
rf_model <- randomForest(track_genre ~ danceability + loudness + key, data = train_data)
importance_values <- importance(rf_model)
importance_df <- data.frame(
  Feature = rownames(importance_values), 
  Importance = importance_values[, "MeanDecreaseGini"] )
importance_df$Importance <- importance_df$Importance / sum(importance_df$Importance)
print(importance_df)

#11. Visualise the importance of features You can use a bar chart to display these weights more intuitively:
library(ggplot2)
importance_df <- data.frame(
  Feature = c("danceability", "loudness", "key"),
  Importance = c(0.3843401, 0.4325703, 0.1830897)
)
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance in Random Forest", x = "Feature", y = "Importance") +
  theme_minimal()

#12. The following is the R code used to generate the heatmap of feature correlations.
library(ggplot2)
library(reshape2)
library(corrplot)
dataset <- read.csv("dataset.csv")
numeric_features <- dataset[, c("danceability", "energy", "loudness", "key", "speechiness", 
                                "acousticness", "instrumentalness", "liveness", "valence", 
                                "tempo")]
correlation_matrix <- cor(numeric_features, use = "complete.obs")
correlation_melt <- melt(correlation_matrix)
ggplot(data = correlation_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Feature Correlation Heatmap",
       x = "Features",
       y = "Features")

#13. The following is an R code for plotting box plots comparing the distributions of features across genres.
library(ggplot2)
library(dplyr)
library(tidyr)
data <- read.csv("dataset.csv")
selected_features <- data %>%
  select(track_genre, danceability, loudness, key)
long_data <- selected_features %>%
  pivot_longer(cols = c(danceability, loudness, key),
               names_to = "Feature",
               values_to = "Value")
ggplot(long_data, aes(x = track_genre, y = Value, fill = Feature)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 1) +
  facet_wrap(~Feature, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Comparison of the characteristic distributions of different genres",
    x = "genre",
    y = "eigenvalue",
    fill = "characteristic"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top")

#14.R Boxplot comparing the distribution of different genre characteristics, including the characteristic columns danceability, loudness and key, valence
library(ggplot2)
library(reshape2)
library(tidyr)
data <- read.csv("dataset.csv")
selected_features <- data[, c("danceability", "loudness", "key", "valence", "track_genre")]
long_data <- melt(selected_features, id.vars = "track_genre", 
                  variable.name = "Feature", value.name = "Value")
boxplot <- ggplot(long_data, aes(x = track_genre, y = Value, fill = Feature)) +
  geom_boxplot(outlier.size = 0.8, alpha = 0.8) +
  facet_wrap(~ Feature, scales = "free") + 
  labs(title = "Box plot of the distribution of characteristics of different genres",
       x = "genre (Track Genre)", 
       y = "eigenvalue (Feature Value)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3", guide = "none") 
print(boxplot)

#15. K-Means clustering
library(ggplot2)
library(cluster)
set.seed(42)
data <- data.frame(
  danceability = runif(100, 0.5, 0.9),
  energy = runif(100, 0.4, 0.8),
  valence = runif(100, 0.3, 0.7))
data_scaled <- scale(data)
wss <- sapply(1:10, function(k) {  kmeans(data_scaled, centers = k, nstart = 10)$tot.withinss})plot(1:10, wss, type = "b", pch = 19, frame = FALSE,      xlab = "Number of Clusters (k)", ylab = "Total Within-Cluster Sum of Squares")
kmeans_model <- kmeans(data_scaled, centers = 3, nstart = 25)data$cluster <- as.factor(kmeans_model$cluster)
ggplot(data, aes(x = danceability, y = energy, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering of Music Data", x = "Danceability", y = "Energy") +theme_minimal()

#16. Generate the cluster column
library(stats)
library(dplyr)
selected_features <- spotify_data %>%
  select(danceability, energy, valence, tempo, loudness, acousticness, instrumentalness, liveness)
scaled_features <- scale(selected_features)
set.seed(42)  
kmeans_result <- kmeans(scaled_features, centers = 3, nstart = 25)
spotify_data$cluster <- as.factor(kmeans_result$cluster)
head(spotify_data)

#17. Cluster results
library(dplyr)
cluster_means <- spotify_data %>%
  group_by(cluster) %>%
  summarise(across(c("danceability", "energy", "valence", "tempo", "loudness", 
                     "acousticness", "instrumentalness", "liveness"), mean, na.rm = TRUE))
cluster_sizes <- spotify_data %>%
  group_by(cluster) %>%
  summarise(Sample_Size = n())
cluster_analysis <- cluster_means %>%
  left_join(cluster_sizes, by = "cluster")
print(cluster_analysis)

#18. Cluster visualisation
library(ggplot2)
library(tidyr)
long_data <- cluster_analysis %>%
  pivot_longer(cols = -c(cluster, Sample_Size), names_to = "Feature", values_to = "Mean_Value")
ggplot(long_data, aes(x = Feature, y = Mean_Value, fill = cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cluster Feature Means", x = "Feature", y = "Mean Value", fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(spotify_data, aes(x = danceability, y = energy, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "Danceability vs Energy by Cluster", x = "Danceability", y = "Energy") +
  theme_minimal()


#19.
library(dplyr)
library(tidyr)
features <- c("danceability", "energy", "valence")
summary_stats <- spotify_data %>%
  select(all_of(features)) %>%
  summarise_all(list(
    Mean = mean,
    Median = median,
    StdDev = sd,
    Min = min,
    Max = max
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Feature", "Statistic"),
    names_sep = "_",
    values_to = "Value"
  )
print(summary_stats)

