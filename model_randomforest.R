library(caret)
library(randomForest)

data <- read.csv('./telecom.csv')

factor_columns <- c('gender', 'Partner', 'Dependents', 'PhoneService', 'MultipleLines',
                    'InternetService', 'OnlineSecurity', 'OnlineBackup', 'DeviceProtection',
                    'TechSupport', 'StreamingTV', 'StreamingMovies', 'Contract',
                    'PaperlessBilling', 'PaymentMethod', 'Churn')
data[factor_columns] <- lapply(data[factor_columns], factor)

data$TotalCharges[is.na(data$TotalCharges)] <- 0

set.seed(123)
trainIndex <- createDataPartition(data$Churn, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

fitControl <- trainControl(
  method = "adaptive_cv",          
  number = 5,                      
  repeats = 5,                    
  search = "random",               
  adaptive = list(                
    min = 5,                       
    alpha = 0.05,                 
    method = "gls",                
    complete = TRUE                
  )
)


grid <- expand.grid(mtry = c(2, 4, 6))


set.seed(123)
rf_model <- train(
  Churn ~ .,
  data = trainData,
  method = "rf",
  trControl = fitControl,
  tuneGrid = grid,
  metric = "Accuracy",
  ntree = 500
)


print(rf_model$bestTune)


predictions <- predict(rf_model, newdata = testData)


cm <- confusionMatrix(predictions, testData$Churn)

print(cm)