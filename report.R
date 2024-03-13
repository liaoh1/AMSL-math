packages <- c("tidyverse", "caret", "dplyr", "tidyr", "randomForest", "keras", "recipes", "glmnet","ggplot2","pROC")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
source('./data.R')
source('./model_cnn.R')
source('./model_l.R')
source('./model_randomforest.R')

