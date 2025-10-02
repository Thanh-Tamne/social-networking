# =============================================================================
# PHÂN TÍCH CẢM XÚC ĐÁNH GIÁ SẢN PHẨM AMAZON - SO SÁNH 3 MÔ HÌNH
# =============================================================================

# Cài đặt và tải thư viện
if(!require(readr)) install.packages("readr")
if(!require(pander)) install.packages("pander")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tm)) install.packages("tm")
if(!require(caret)) install.packages("caret")
if(!require(LiblineaR)) install.packages("LiblineaR")
if(!require(e1071)) install.packages("e1071")
if(!require(gmodels)) install.packages("gmodels")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(randomForest)) install.packages("randomForest")
if(!require(class)) install.packages("class")
if(!require(gridExtra)) install.packages("gridExtra")

library(readr)
library(pander)
library(dplyr)
library(tm)
library(caret)
library(LiblineaR)
library(e1071)
library(gmodels)
library(ggplot2)
library(randomForest)
library(class)
library(gridExtra)

# Đọc dữ liệu
product_review <- read_csv("amazon_reviews.csv")
head(product_review)

# Hiển thị bảng đẹp
pandoc.table(product_review[2:4, 1:3], 
             justify = c('left', 'left', 'center'), style = 'grid')

# Phân bố rating
table(product_review$rating)

# Chuyển đổi thành binary classification (1: positive, 0: negative, loại bỏ rating 3)
product_review <- product_review %>% 
  filter(rating != 3) %>% 
  mutate(rating_new = if_else(rating >= 4, 1, 0))

# Chia dữ liệu training và test
product_review_training <- product_review[1:150, ]
test_review_data <- product_review[151:174, ]

# Tạo corpus và Document Term Matrix cho training
corpus_toy <- Corpus(VectorSource(product_review_training$review))
tdm_toy <- DocumentTermMatrix(corpus_toy, 
                              list(removePunctuation = TRUE, 
                                   removeNumbers = TRUE))

# Chuyển thành data frame và thêm biến mục tiêu
training_set_toy <- as.matrix(tdm_toy)
training_set_toy <- cbind(training_set_toy, product_review_training$rating_new)
colnames(training_set_toy)[ncol(training_set_toy)] <- "y"
training_set_toy <- as.data.frame(training_set_toy)
training_set_toy$y <- as.factor(training_set_toy$y)

# Tạo DTM cho test set
test_corpus <- Corpus(VectorSource(test_review_data$review))
test_tdm <- DocumentTermMatrix(test_corpus, 
                               control = list(dictionary = Terms(tdm_toy)))
test_tdm <- as.matrix(test_tdm)
test_set_toy <- as.data.frame(test_tdm)


# =============================================================================
# MÔ HÌNH 1: SVM (SUPPORT VECTOR MACHINE)
# =============================================================================

cat("\n=== HUẤN LUYỆN MÔ HÌNH SVM ===\n")
svm_model <- train(y ~ ., 
                   data = training_set_toy, 
                   method = 'svmLinear',
                   trControl = trainControl(method = "none"))

# Dự đoán SVM
svm_predictions <- predict(svm_model, newdata = test_set_toy)

# Đánh giá SVM bằng confusionMatrix (thư viện caret)
svm_cm <- confusionMatrix(svm_predictions, as.factor(test_review_data$rating_new))
cat("\n=== KẾT QUẢ ĐÁNH GIÁ SVM ===\n")
print(svm_cm)


# =============================================================================
# MÔ HÌNH 2: KNN (K-NEAREST NEIGHBORS)
# =============================================================================

cat("\n=== HUẤN LUYỆN MÔ HÌNH KNN ===\n")
knn_model <- train(y ~ ., 
                   data = training_set_toy, 
                   method = 'knn',
                   trControl = trainControl(method = "none"),
                   tuneGrid = data.frame(k = 5))

# Dự đoán KNN
knn_predictions <- predict(knn_model, newdata = test_set_toy)

# Đánh giá KNN bằng confusionMatrix
knn_cm <- confusionMatrix(knn_predictions, as.factor(test_review_data$rating_new))
cat("\n=== KẾT QUẢ ĐÁNH GIÁ KNN ===\n")
print(knn_cm)


# =============================================================================
# MÔ HÌNH 3: RANDOM FOREST
# =============================================================================

cat("\n=== HUẤN LUYỆN MÔ HÌNH RANDOM FOREST ===\n")
rf_model <- train(y ~ ., 
                  data = training_set_toy, 
                  method = 'rf',
                  trControl = trainControl(method = "none"),
                  tuneGrid = data.frame(mtry = 50))

# Dự đoán Random Forest
rf_predictions <- predict(rf_model, newdata = test_set_toy)

# Đánh giá Random Forest bằng confusionMatrix
rf_cm <- confusionMatrix(rf_predictions, as.factor(test_review_data$rating_new))
cat("\n=== KẾT QUẢ ĐÁNH GIÁ RANDOM FOREST ===\n")
print(rf_cm)


# =============================================================================
# SO SÁNH KẾT QUẢ 3 MÔ HÌNH
# =============================================================================

# Trích xuất các chỉ số từ confusionMatrix
comparison_table <- data.frame(
  Model = c("SVM", "KNN", "Random Forest"),
  Accuracy = c(
    round(svm_cm$overall['Accuracy'] * 100, 1),
    round(knn_cm$overall['Accuracy'] * 100, 1),
    round(rf_cm$overall['Accuracy'] * 100, 1)
  ),
  Sensitivity = c(
    round(svm_cm$byClass['Sensitivity'] * 100, 1),
    round(knn_cm$byClass['Sensitivity'] * 100, 1),
    round(rf_cm$byClass['Sensitivity'] * 100, 1)
  ),
  Specificity = c(
    round(svm_cm$byClass['Specificity'] * 100, 1),
    round(knn_cm$byClass['Specificity'] * 100, 1),
    round(rf_cm$byClass['Specificity'] * 100, 1)
  ),
  Precision = c(
    round(svm_cm$byClass['Pos Pred Value'] * 100, 1),
    round(knn_cm$byClass['Pos Pred Value'] * 100, 1),
    round(rf_cm$byClass['Pos Pred Value'] * 100, 1)
  ),
  F1_Score = c(
    round(svm_cm$byClass['F1'] * 100, 1),
    round(knn_cm$byClass['F1'] * 100, 1),
    round(rf_cm$byClass['F1'] * 100, 1)
  )
)

cat("\n======================================================\n")
cat("=== BẢNG SO SÁNH CHI TIẾT 3 MÔ HÌNH ===\n")
cat("======================================================\n")
print(comparison_table)

# Hiển thị bảng đẹp hơn bằng pander
cat("\n")
pandoc.table(comparison_table, 
             justify = c('left', 'center', 'center', 'center', 'center', 'center'),
             style = 'grid',
             caption = "So Sánh Hiệu Suất 3 Mô Hình Machine Learning")


# =============================================================================
# BIỂU ĐỒ SO SÁNH
# =============================================================================

# Chuyển dữ liệu sang dạng long format để vẽ biểu đồ
comparison_long <- comparison_table %>%
  tidyr::pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

# Biểu đồ so sánh các metrics
metrics_plot <- ggplot(comparison_long, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(Value, "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("SVM" = "#3498db", "KNN" = "#e74c3c", "Random Forest" = "#2ecc71")) +
  labs(
    title = "So Sánh Các Chỉ Số Đánh Giá Giữa 3 Mô Hình",
    x = "Metrics",
    y = "Value (%)",
    fill = "Model"
  ) +
  ylim(0, 110) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold")
  )

print(metrics_plot)

# Biểu đồ so sánh chỉ Accuracy
accuracy_plot <- ggplot(comparison_table, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Accuracy, "%")), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("SVM" = "#3498db", "KNN" = "#e74c3c", "Random Forest" = "#2ecc71")) +
  labs(
    title = "So Sánh Độ Chính Xác (Accuracy) Giữa 3 Mô Hình",
    x = "Model",
    y = "Accuracy (%)",
    fill = "Model"
  ) +
  ylim(0, 110) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

print(accuracy_plot)


# =============================================================================
# CONFUSION MATRIX CHO TỪNG MÔ HÌNH
# =============================================================================

# Hàm tạo confusion matrix plot
create_confusion_matrix_plot <- function(cm_object, model_name) {
  # Lấy confusion matrix table
  cm_table <- cm_object$table
  cm_df <- as.data.frame(cm_table)
  colnames(cm_df) <- c("Prediction", "Reference", "Freq")
  
  # Chuyển đổi 0/1 thành Negative/Positive
  cm_df <- cm_df %>%
    mutate(
      Prediction = if_else(Prediction == "1", "Positive", "Negative"),
      Reference = if_else(Reference == "1", "Positive", "Negative")
    )
  
  # Lấy accuracy
  accuracy <- round(cm_object$overall['Accuracy'] * 100, 1)
  
  plot <- ggplot(cm_df, aes(x = Prediction, y = Reference, fill = Freq)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = Freq), size = 8, fontface = "bold", color = "white") +
    scale_fill_gradient(low = "#3498db", high = "#e74c3c") +
    labs(
      title = paste0(model_name, "\nAccuracy: ", accuracy, "%"),
      x = "Predicted",
      y = "Actual",
      fill = "Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
      axis.text = element_text(size = 9, face = "bold"),
      axis.title = element_text(size = 10, face = "bold"),
      legend.position = "none"
    )
  
  return(plot)
}

# Tạo confusion matrix plot cho từng mô hình
svm_cm_plot <- create_confusion_matrix_plot(svm_cm, "SVM")
knn_cm_plot <- create_confusion_matrix_plot(knn_cm, "KNN")
rf_cm_plot <- create_confusion_matrix_plot(rf_cm, "Random Forest")

# Hiển thị tất cả confusion matrix cùng lúc
grid.arrange(svm_cm_plot, knn_cm_plot, rf_cm_plot, ncol = 3,
             top = grid::textGrob("Confusion Matrix - So Sánh 3 Mô Hình", 
                                  gp = grid::gpar(fontsize = 15, fontface = "bold")))


# =============================================================================
# XÁC ĐỊNH MÔ HÌNH TỐT NHẤT
# =============================================================================

best_model_idx <- which.max(comparison_table$Accuracy)
best_model_name <- comparison_table$Model[best_model_idx]
best_accuracy <- comparison_table$Accuracy[best_model_idx]

cat("\n======================================================\n")
cat("=== MÔ HÌNH TỐT NHẤT ===\n")
cat("======================================================\n")
cat("Model:", best_model_name, "\n")
cat("Accuracy:", best_accuracy, "%\n")
cat("Sensitivity (Recall):", comparison_table$Sensitivity[best_model_idx], "%\n")
cat("Specificity:", comparison_table$Specificity[best_model_idx], "%\n")
cat("Precision:", comparison_table$Precision[best_model_idx], "%\n")
cat("F1-Score:", comparison_table$F1_Score[best_model_idx], "%\n")
cat("======================================================\n")

# Lấy confusion matrix object của mô hình tốt nhất
best_cm <- switch(best_model_name,
                  "SVM" = svm_cm,
                  "KNN" = knn_cm,
                  "Random Forest" = rf_cm)

# Hiển thị confusion matrix chi tiết của mô hình tốt nhất
cat("\n=== CONFUSION MATRIX CHI TIẾT CỦA MÔ HÌNH TỐT NHẤT ===\n")
print(best_cm$table)

# Vẽ confusion matrix lớn cho mô hình tốt nhất
best_cm_df <- as.data.frame(best_cm$table)
colnames(best_cm_df) <- c("Prediction", "Reference", "Freq")

best_cm_df <- best_cm_df %>%
  mutate(
    Prediction = if_else(Prediction == "1", "Positive", "Negative"),
    Reference = if_else(Reference == "1", "Positive", "Negative")
  )

best_cm_plot <- ggplot(best_cm_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white", linewidth = 2) +
  geom_text(aes(label = Freq), size = 14, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "#3498db", high = "#e74c3c") +
  labs(
    title = paste0("Confusion Matrix - ", best_model_name, " (Best Model)"),
    subtitle = paste0("Accuracy: ", best_accuracy, "% | F1-Score: ", 
                      comparison_table$F1_Score[best_model_idx], "%"),
    x = "Predicted Sentiment",
    y = "Actual Sentiment",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14, face = "bold")
  )

print(best_cm_plot)