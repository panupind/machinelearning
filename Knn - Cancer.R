# KNN

#The data set consists of 100 observations and 10 variables 
#(out of which 8 numeric variables and one categorical 
#variable and is ID) which are as follows:
  
#Radius
#Texture
#Perimeter
#Area
#Smoothness
#Compactness
#Symmetry
#Fractal dimension

data <- read.csv(file.choose())


str(data)

#remove the first variable(id) from the data set.
data <- data[-1]  

# Balance of class info
table(data$diagnosis_result)


# Normalize Data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

data_n <- as.data.frame(lapply(data[2:9], normalize))

summary(data_n$radius)


# Split into train and test data - Independant Variables
data_train <- data_n[1:65,]
data_test <- data_n[66:100,]

# Split into train and test data - dependant Variable
data_train_labels <- data[1:65, 1]
data_test_labels <- data[66:100, 1]

library(class)

# Training the model
data_test_pred <- knn(train = data_train, test = data_test,cl = data_train_labels, k=10)


library(caret)
confusionMatrix(data_test_pred,
                data_test_labels, 
                positive = "M",
                mode="everything")
