
install.packages('mlbench')
install.packages('tidyverse')

library(tidyverse)
library(caret)
library(xgboost)
library(mlbench)
library(dplyr)

# Load the data and remove NAs
data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
PimaIndiansDiabetes2 
# Inspect the data

sample_n(PimaIndiansDiabetes2, 3)
# Split the data into training and test set
set.seed(123)
training.samples <- PimaIndiansDiabetes2$diabetes %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- PimaIndiansDiabetes2[training.samples, ]
test.data <- PimaIndiansDiabetes2[-training.samples, ]


# Fit the model on the training set
set.seed(123)
model <- train( diabetes ~., data = train.data, method = "xgbTree", trControl = trainControl("cv", number = 10) )

# Best tuning parameter
model$bestTune


# Make predictions on the test data
predicted.classes <- model %>% predict(test.data)
predicted.classes
head(predicted.classes)



# Compute model prediction accuracy rate
mean(predicted.classes == test.data$diabetes)

varImp(model)

