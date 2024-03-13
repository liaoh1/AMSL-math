library(keras)
library(dplyr)
library(tidyr)
library(caret)
library(recipes)
library(ggplot2)

data_path <- "./telecom.csv"
data <- read.csv(data_path, stringsAsFactors = FALSE)

categorical_columns <- c('gender', 'Partner', 'Dependents', 'PhoneService',
                         'MultipleLines', 'InternetService', 'OnlineSecurity',
                         'OnlineBackup', 'DeviceProtection', 'TechSupport',
                         'StreamingTV', 'StreamingMovies', 'Contract',
                         'PaperlessBilling', 'PaymentMethod', 'Churn')

data[categorical_columns] <- lapply(data[categorical_columns], factor)

data$TotalCharges <- as.numeric(replace(data$TotalCharges, data$TotalCharges == "", 0))
data$TotalCharges[is.na(data$TotalCharges)] <- 0

set.seed(123)
trainIndex <- createDataPartition(data$Churn, p = .8, list = FALSE, times = 1)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

recipe_obj <- recipe(Churn ~ ., data = trainData) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  prep(training = trainData, retain = TRUE)  

train_processed <- bake(recipe_obj, new_data = trainData)
test_processed <- bake(recipe_obj, new_data = testData)

x_train <- as.matrix(train_processed[, -which(colnames(train_processed) == "Churn")])
y_train <- as.matrix(train_processed$Churn)

x_test <- as.matrix(test_processed[, -which(colnames(test_processed) == "Churn")])
y_test <- as.matrix(test_processed$Churn)

y_train <- ifelse(y_train == "Yes", 1, 0)
y_test <- ifelse(y_test == "Yes", 1, 0)

library(keras)

add_residual_block <- function(input, units) {
  x <- layer_dense(input, units = units, activation = 'relu') %>%
    layer_dropout(rate = 0.5)
  x <- layer_dense(x, units = units, activation = 'relu') %>%
    layer_dropout(rate = 0.5)
  x <- layer_add(list(input, x))
  x <- layer_activation(x, activation = 'relu')
  return(x)
}

inputs <- layer_input(shape = c(30))

x <- layer_dense(inputs, units = 64, activation = 'relu') %>%
  layer_dropout(rate = 0.5)

x <- add_residual_block(x, units = 64)
x <- add_residual_block(x, units = 64)

outputs <- layer_dense(x, units = 1, activation = 'sigmoid')

model <- keras_model(inputs = inputs, outputs = outputs)

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

early_stop <- callback_early_stopping(
  monitor = "val_loss",
  patience = 10, 
  restore_best_weights = TRUE
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 50,
  batch_size = 64,
  validation_split = 0.2,
  callbacks = list(early_stop) 
)

model %>% evaluate(x_test, y_test)

predictions <- model %>% predict(x_test)

predicted_classes <- ifelse(predictions > 0.5, "Yes", "No")

if (length(predicted_classes) == length(test_processed$Churn)) {
  confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(test_processed$Churn))
  print(confusion_matrix)
} else {
  stop("The dimensions of the predicted classes and actual labels do not match.")
}

prob_predictions <- model %>% predict(x_test)
y_test_numeric <- as.numeric(y_test == 1)


cal_data <- data.frame(
  'prob' = prob_predictions, 
  'obs' = y_test_numeric
)


cal_data$bin <- cut(cal_data$prob, breaks=seq(0, 1, by=0.1), include.lowest=TRUE, right=TRUE)
cal_data_grouped <- cal_data %>%
  group_by(bin) %>%
  summarise(
    mean_prob = mean(prob),
    freq = n(),
    mean_obs = mean(obs)
  ) %>%
  mutate(
    loess_fit = loess(mean_obs ~ mean_prob, data=., span=0.75)$fitted
  )


calibration_plot <- ggplot(cal_data_grouped, aes(x=mean_prob, y=mean_obs)) +
  geom_point() +
  geom_line(aes(y = loess_fit), color='blue') +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Predicted Probability", y = "Observed Frequency") +
  ggtitle("Calibration Plot")


ggsave("calibration_plot.png", plot = calibration_plot, width = 10, height = 6)