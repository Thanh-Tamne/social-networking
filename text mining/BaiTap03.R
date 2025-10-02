# =============================================================================
# PHÂN TÍCH CẢM XÚC ĐÁNH GIÁ SẢN PHẨM AMAZON BẰNG SVM
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

library(readr)
library(pander)
library(dplyr)
library(tm)
library(caret)
library(LiblineaR)
library(e1071)
library(gmodels)
library(ggplot2)

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

# Huấn luyện mô hình SVM
review_toy_model <- train(y ~ ., 
                          data = training_set_toy, 
                          method = 'svmLinear',
                          trControl = trainControl(method = "none"))

# Tạo DTM cho test set
test_corpus <- Corpus(VectorSource(test_review_data$review))
test_tdm <- DocumentTermMatrix(test_corpus, 
                               control = list(dictionary = Terms(tdm_toy)))
test_tdm <- as.matrix(test_tdm)

# Dự đoán
model_toy_result <- predict(review_toy_model, newdata = test_tdm)

# Đánh giá kết quả
check_accuracy <- as.data.frame(cbind(
  prediction = as.integer(as.character(model_toy_result)),
  rating = test_review_data$rating_new
))

check_accuracy$accuracy <- if_else(check_accuracy$prediction == check_accuracy$rating, 1, 0)

# Tính độ chính xác
accuracy_table <- round(prop.table(table(check_accuracy$accuracy)), 3)
accuracy_rate <- round(accuracy_table[2] * 100, 1)

cat("\n=== ĐỘ CHÍNH XÁC:", accuracy_rate, "% ===\n")
print(accuracy_table)

# Bảng chéo
CrossTable(x = test_review_data$rating_new, 
           y = as.integer(as.character(model_toy_result)),
           prop.chisq = FALSE)

# =============================================================================
# VẼ CONFUSION MATRIX
# =============================================================================

# Tạo data frame cho confusion matrix
check_accuracy <- check_accuracy %>%
  mutate(
    pred_sentiment = if_else(prediction == 1, "Positive", "Negative"),
    actual_sentiment = if_else(rating == 1, "Positive", "Negative")
  )

confusion_matrix <- table(
  Actual = check_accuracy$actual_sentiment,
  Predicted = check_accuracy$pred_sentiment
)
confusion_df <- as.data.frame(confusion_matrix)

# Vẽ confusion matrix
confusion_plot <- ggplot(confusion_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = Freq), size = 12, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "#3498db", high = "#e74c3c") +
  labs(
    title = paste0("Confusion Matrix - Accuracy: ", accuracy_rate, "%"),
    x = "Predicted Sentiment",
    y = "Actual Sentiment",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14, face = "bold")
  )

print(confusion_plot)
confusion_plot
