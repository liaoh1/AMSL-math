library(caret) 
library(dplyr) 
library(tidyr) 

data <- read.csv('./telecom.csv')

categorical_columns <- c('gender', 'Partner', 'Dependents', 'PhoneService', 'MultipleLines', 'InternetService',
                         'OnlineSecurity', 'OnlineBackup', 'DeviceProtection', 'TechSupport', 'StreamingTV',
                         'StreamingMovies', 'Contract', 'PaperlessBilling', 'PaymentMethod', 'Churn')
data[categorical_columns] <- lapply(data[categorical_columns], factor)


data$TotalCharges[is.na(data$TotalCharges)] <- 0

set.seed(123) 
trainIndex <- createDataPartition(data$Churn, p = .8,list = FALSE, times = 1)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

fitControl <- trainControl(method = 'cv',
                           number = 10)  

model <- train(Churn ~ ., 
               data = trainData,
               method = 'glmnet',  
               family = 'binomial', 
               trControl = fitControl)

predictions <- predict(model, newdata = testData)
cm <- confusionMatrix(predictions, testData$Churn)

print(cm)