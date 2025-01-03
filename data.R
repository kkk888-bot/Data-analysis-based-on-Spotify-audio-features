#1. 数据加载与预处理   加载数据并查看数据框的基本信息：
# 加载必要的库
library(ggplot2)
library(dplyr)
# 读取数据（文件为 'dataset.csv'）
spotify_data<-read.csv("dataset.csv")
# 检查数据框基本结构
str(spotify_data)
summary(spotify_data)
# 处理多艺术家分隔符
spotify_data$artists <- strsplit(as.character(spotify_data$artists), ";")

#2. 探索性数据分析 (EDA)
# 安装并加载需要的库
install.packages("ggplot2")
install.packages("dplyr")
install.packages("caret")  # 用于混淆矩阵
library(ggplot2)
library(dplyr)
library(caret)
#2.1 流派与舞蹈性分布   绘制每个流派的舞蹈性分布箱线图：
ggplot(spotify_data, aes(x = track_genre, y = danceability, fill = track_genre)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Danceability Across Music Genres",
    x = "Genre",
    y = "Danceability"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#2.2 音乐流派中的流行度分布
ggplot(spotify_data, aes(x = track_genre, y = popularity, fill = track_genre)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Popularity Distribution Across Music Genres",
    x = "Genre",
    y = "Popularity"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#2.3 响度与能量的关系  绘制响度与能量的散点图：
ggplot(spotify_data, aes(x = loudness, y = energy, color = track_genre)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Loudness vs Energy by Genre",
    x = "Loudness (dB)",
    y = "Energy",
    color = "Genre"
  )


#------------------------------------
#以下是完整的 R 代码示例，使用随机森林对你提供的数据集进行分类，包括数据预处理、模型训练和评估。
#假设目标是基于音乐特征（如 danceability, energy, 等）预测 track_genre。
#1 加载必要的库
install.packages("randomForest")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)
#2. 加载和预处理数据
# 读取数据文件 "dataset.csv"
dataset <- read.csv("dataset.csv")
# 检查数据结构
str(dataset)
summary(dataset)
# 确保目标变量 `track_genre` 是因子类型
dataset$track_genre <- as.factor(dataset$track_genre)
# 删除非数值或无关的列（如 `track_id`, `track_name`, `album_name`, `artists`）
dataset_clean <- dataset %>%
  select(-track_id, -track_name, -album_name, -artists)
# 检查并移除缺失值
dataset_clean <- na.omit(dataset_clean)

#3. 划分训练集和测试集
# 分割数据集：80% 为训练集，20% 为测试集
set.seed(42)  # 设置随机种子以确保可重复性
train_index <- createDataPartition(dataset_clean$track_genre, p = 0.8, list = FALSE)
train_data <- dataset_clean[train_index, ]
test_data <- dataset_clean[-train_index, ]
#4. 训练随机森林模型
# 训练随机森林模型
set.seed(42)
rf_model <- randomForest(
  track_genre ~ .,  # 使用所有特征预测 track_genre
  data = train_data,
  ntree = 100,      # 决策树数量
  mtry = 3,         # 每次分裂时的随机特征数量
  importance = TRUE # 启用特征重要性
)
# 查看模型结果
print(rf_model)
# 查看 OOB（Out-of-Bag）错误率
plot(rf_model)

#5. 评估模型性能  
#5.1 在测试集上预测
# 对测试集进行预测
predictions <- predict(rf_model, newdata = test_data)
# 查看前几项预测结果
head(predictions)
#5.2 计算混淆矩阵和准确率
# 混淆矩阵
conf_matrix <- confusionMatrix(predictions, test_data$track_genre)
print(conf_matrix)
# 输出准确率
accuracy <- conf_matrix$overall["Accuracy"]
print(paste("Model Accuracy:", accuracy))
#5.3 可视化混淆矩阵
# 转换混淆矩阵为数据框
conf_matrix_data <- as.data.frame.table(conf_matrix$table)
# 绘制混淆矩阵热图
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

# 计算特征重要性
importance_values <- importance(rf_model)
# 转换为数据框，提取重要性指标
importance_df <- data.frame(
  Feature = rownames(importance_values), 
  Importance = importance_values[, "MeanDecreaseGini"] # 基于 Gini 的重要性
)
# 归一化权重为比例
importance_df$Importance <- importance_df$Importance / sum(importance_df$Importance)
# 查看结果
print(importance_df)

#————————————————————————————————————————————————------------------------>>>>

# 加载必要的包
library(caret)
library(tidyverse)

# 读取数据集
data <- read.csv("dataset.csv")

# 选择需要的特征和目标变量
data_selected <- data %>%
  select(track_genre, loudness, danceability, key)

# 将目标变量转换为因子类型（分类问题）
data_selected$track_genre <- as.factor(data_selected$track_genre)

# 划分训练集和测试集
set.seed(123) # 固定随机种子
train_index <- createDataPartition(data_selected$track_genre, p = 0.8, list = FALSE)
train_data <- data_selected[train_index, ]
test_data <- data_selected[-train_index, ]

# 训练逻辑回归模型
logistic_model <- glm(track_genre ~ loudness + danceability + key, 
                      data = train_data, 
                      family = binomial)

# 查看模型系数（重要性分析）
summary(logistic_model)

# 对测试集进行预测
test_data$predicted <- predict(logistic_model, newdata = test_data, type = "response")
test_data$predicted_class <- ifelse(test_data$predicted > 0.5, 1, 0) # 二分类问题的简单处理

# 计算混淆矩阵
conf_matrix <- confusionMatrix(as.factor(test_data$predicted_class), 
                               as.factor(ifelse(test_data$track_genre == levels(test_data$track_genre)[1], 0, 1)))

print(conf_matrix)

# 重要性分析（基于标准化回归系数）
importance <- abs(coef(logistic_model)[-1]) # 去掉截距项
names(importance) <- c("loudness", "danceability", "key")
importance <- importance / sum(importance) # 归一化为权重比例

# 打印重要性分析
importance_df <- data.frame(Feature = names(importance), Importance = importance)
importance_df <- importance_df[order(-importance_df$Importance), ]
print(importance_df)

# 可视化特征重要性
library(ggplot2)
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance (Logistic Regression)",
       x = "Features", 
       y = "Importance") +
  theme_minimal()


#————————————————————————————————————————————————————————————————————————————————————————》〉》
# 加载必要库
library(caret)
# 示例：假设 `y_true` 为真实标签，`y_pred` 为模型预测结果
y_true <- test_data$track_genre
y_pred <- predict(rf_model, test_data)
# 生成混淆矩阵
conf_matrix <- confusionMatrix(y_pred, y_true)
# 提取性能指标
accuracy <- conf_matrix$overall['Accuracy'] # 准确率
class_metrics <- conf_matrix$byClass # 按类别的指标
# 计算平均指标
precision <- mean(class_metrics[, "Precision"], na.rm = TRUE)
recall <- mean(class_metrics[, "Recall"], na.rm = TRUE)
f1_score <- mean(class_metrics[, "F1"], na.rm = TRUE)
# 创建性能表格
performance_table <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1-Score"),
  Value = c(accuracy, precision, recall, f1_score)
)
# 打印性能表格
print(performance_table)
#-------------------------------------------------------------
# 加载随机森林库
library(randomForest)
# 假设数据已加载并分为训练集和测试集
# 训练随机森林模型
rf_model <- randomForest(track_genre ~ danceability + loudness + key, data = train_data)
# 计算特征重要性
importance_values <- importance(rf_model)
# 转换为数据框，提取重要性指标
importance_df <- data.frame(
  Feature = rownames(importance_values), 
  Importance = importance_values[, "MeanDecreaseGini"] )# 基于 Gini 的重要性

# 归一化权重为比例
importance_df$Importance <- importance_df$Importance / sum(importance_df$Importance)
# 查看结果
print(importance_df)
#------------------------------------------------------------------------------------
#可视化特征重要性   你可以用条形图更直观地展示这些权重：
library(ggplot2)
# 数据框
importance_df <- data.frame(
  Feature = c("danceability", "loudness", "key"),
  Importance = c(0.3843401, 0.4325703, 0.1830897)
)
# 绘图
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance in Random Forest", x = "Feature", y = "Importance") +
  theme_minimal()
#---------------------------------------------------------------------------------------
#以下是生成特征相关性热图的 R 代码，假设你已经加载了数据集并对其进行了基本清理：
# 加载必要的库
library(ggplot2)
library(reshape2)
library(corrplot)
# 读取数据
dataset <- read.csv("dataset.csv")
# 选择数值型特征进行相关性分析
numeric_features <- dataset[, c("danceability", "energy", "loudness", "key", "speechiness", 
                                "acousticness", "instrumentalness", "liveness", "valence", 
                                "tempo")]
# 计算相关系数矩阵
correlation_matrix <- cor(numeric_features, use = "complete.obs")
# 将相关矩阵转换为长格式，便于绘制
correlation_melt <- melt(correlation_matrix)
# 绘制热图
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
#---------------------------------------------------------------------------------------
#以下是使用 R 代码绘制比较不同流派特征分布的箱线图。
#假设数据集为 dataset.csv，包含特征列 danceability、loudness 和 key，
#以及目标变量 track_genre。
# 加载必要的库
library(ggplot2)
library(dplyr)
library(tidyr)
# 读取数据集
data <- read.csv("dataset.csv")
# 提取感兴趣的特征和目标变量
selected_features <- data %>%
  select(track_genre, danceability, loudness, key)
# 将数据转换为长格式以便于绘制
long_data <- selected_features %>%
  pivot_longer(cols = c(danceability, loudness, key),
               names_to = "Feature",
               values_to = "Value")
# 绘制箱线图
ggplot(long_data, aes(x = track_genre, y = Value, fill = Feature)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 1) +
  facet_wrap(~Feature, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "不同流派的特征分布比较",
    x = "流派",
    y = "特征值",
    fill = "特征"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top")
#---------------------------------------------------------------------------------------
#R 比较不同流派特征分配的箱线图包含特征列 danceability、loudness 和 key，valence
# 加载必要的库
library(ggplot2)
library(reshape2)
library(tidyr)
# 读取数据
data <- read.csv("dataset.csv")
# 选择需要的特征和目标变量
selected_features <- data[, c("danceability", "loudness", "key", "valence", "track_genre")]
# 将数据从宽表转换为长表格式，便于 ggplot2 使用
long_data <- melt(selected_features, id.vars = "track_genre", 
                  variable.name = "Feature", value.name = "Value")
# 绘制箱线图
boxplot <- ggplot(long_data, aes(x = track_genre, y = Value, fill = Feature)) +
  geom_boxplot(outlier.size = 0.8, alpha = 0.8) +
  facet_wrap(~ Feature, scales = "free") +  # 每个特征单独分面
  labs(title = "不同流派特征分布的箱线图",
       x = "流派 (Track Genre)", 
       y = "特征值 (Feature Value)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3", guide = "none")  # 颜色调节，隐藏图例
# 显示图表
print(boxplot)



#---------------------------------------------------------------------------------------
#K-Means 聚类
# 加载必要库
library(ggplot2)
library(cluster)

# 示例数据集（替换为实际数据）
set.seed(42)
data <- data.frame(
  danceability = runif(100, 0.5, 0.9),
  energy = runif(100, 0.4, 0.8),
  valence = runif(100, 0.3, 0.7))

# 数据标准化data_scaled <- scale(data)

# 确定最佳聚类数（Elbow Method）
wss <- sapply(1:10, function(k) {  kmeans(data_scaled, centers = k, nstart = 10)$tot.withinss})plot(1:10, wss, type = "b", pch = 19, frame = FALSE,      xlab = "Number of Clusters (k)", ylab = "Total Within-Cluster Sum of Squares")

# K-Means 聚类set.seed(42)
kmeans_model <- kmeans(data_scaled, centers = 3, nstart = 25)data$cluster <- as.factor(kmeans_model$cluster)

# 可视化聚类结果
ggplot(data, aes(x = danceability, y = energy, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering of Music Data", x = "Danceability", y = "Energy") +theme_minimal()




#---------------------------------------------------------------------------------------

# 绘制 danceability 的直方图
ggplot(spotify_data, aes(x = danceability)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "blue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Danceability", x = "Danceability", y = "Density") +theme_minimal()

# 绘制 valence 的直方图
ggplot(spotify_data, aes(x = valence)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "green", alpha = 0.7) +
  geom_density(color = "red", size = 1) +labs(title = "Distribution of Valence", x = "Valence", y = "Density") +theme_minimal()
# 绘制 energy 的直方图
ggplot(spotify_data, aes(x = energy)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "blue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Distribution of Energy",
    x = "Energy",
    y = "Density"
  ) +
  theme_minimal()
# 绘制 energy 与 loudness 的散点图
ggplot(spotify_data, aes(x = loudness, y = energy)) +
  geom_point(alpha = 0.7, color = "blue") +
  labs(title = "Scatterplot of Loudness vs Energy", x = "Loudness", y = "Energy") +theme_minimal()

# 绘制 valence 与 danceability 的散点图
ggplot(spotify_data, aes(x = danceability, y = valence)) +
  geom_point(alpha = 0.7, color = "green") +
  labs(title = "Scatterplot of Danceability vs Valence", x = "Danceability", y = "Valence") +theme_minimal()

#---------------------------------------------------------------------------------------
# 加载必要库
library(dplyr)
library(tidyr)

# 选择感兴趣的特征
features <- c("danceability", "energy", "valence")

# 计算统计描述
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

# 打印修正后的表格
print(summary_stats)



#---------------------------------------------------------------------------------------
#聚类结果
# 加载必要的包
library(dplyr)

# 确保 'cluster' 列存在并正确
if (!"cluster" %in% colnames(spotify_data)) {
  stop("请确保 'cluster' 列已包含在数据集中。")
}

# 计算每个簇的特征均值
cluster_means <- spotify_data %>%
  group_by(cluster) %>%
  summarise(across(c("danceability", "energy", "valence", "tempo", "loudness", 
                     "acousticness", "instrumentalness", "liveness"), mean, na.rm = TRUE))

# 添加每个簇的样本数量
cluster_sizes <- spotify_data %>%
  group_by(cluster) %>%
  summarise(Sample_Size = n())

# 合并特征均值和簇内数量
cluster_analysis <- cluster_means %>%
  left_join(cluster_sizes, by = "cluster")

# 打印分析结果
print(cluster_analysis)
#---------------------------------------------------------------------------------------
#生成cluster列
# 加载必要的库
library(stats)
library(dplyr)

# 选择特征
selected_features <- spotify_data %>%
  select(danceability, energy, valence, tempo, loudness, acousticness, instrumentalness, liveness)

# 标准化特征
scaled_features <- scale(selected_features)

# 执行 K-Means 聚类
set.seed(42)  # 设置随机种子，确保结果可重复
kmeans_result <- kmeans(scaled_features, centers = 3, nstart = 25)

# 将聚类结果添加到数据框
spotify_data$cluster <- as.factor(kmeans_result$cluster)

# 查看结果
head(spotify_data)


#---------------------------------------------------------------------------------------
#可视化建议
# 加载必要的包
library(ggplot2)
library(tidyr)

# 将数据转为长格式
long_data <- cluster_analysis %>%
  pivot_longer(cols = -c(cluster, Sample_Size), names_to = "Feature", values_to = "Mean_Value")

# 绘制柱状图
ggplot(long_data, aes(x = Feature, y = Mean_Value, fill = cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cluster Feature Means", x = "Feature", y = "Mean Value", fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 绘制两个特征的散点图
ggplot(spotify_data, aes(x = danceability, y = energy, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "Danceability vs Energy by Cluster", x = "Danceability", y = "Energy") +
  theme_minimal()
#---------------------------------------------------------------------------------------