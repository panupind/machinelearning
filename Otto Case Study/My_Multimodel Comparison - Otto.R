#install.packages("caret")
library(caret)

data <- read.csv(file.choose())

# Normalize Data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

data_n <- as.data.frame(lapply(data[-c(1,95)], normalize))

data_n$target <- data$target

str(data_n)


trainIndex <- createDataPartition(data_n$target,p=0.75,list=FALSE)

head(trainIndex)

data_train <- data_n[trainIndex,]

data_test <- data_n[-trainIndex,]

table(data_train$target)

table(data_test$target)

library(psych)

KMO(cor(data[-c(1,95)]))

#BArtlett's Test of Sphericity - cortest.bartlett
#Kaiser Meyer Olkin

X <- as.matrix(data[-c(1,95)])

PCA <- princomp(~X, scores = TRUE, cor=TRUE)

summary(PCA)

PCA

# Perform Multinomial Logistic Regression

library(nnet)

multinom <- multinom(target ~ ., data = data_train[-1,], maxit=30)

predicted=predict(multinom,data_test,type="class")


# Evaluate Model performance
#install.packages("e1071")
library(e1071)
library(caret)
confusionMatrix(predicted,
                data_test$target,
                mode="everything")



#---------------------------------------------------
  
  # Perform LDA
  #install.packages("DiscriMiner")
  library(DiscriMiner)

# Split data into DV & IV's
X <- data_train[,-94]
Y <- data_train[,94]

Mahalanobis = linDA(X,Y)

#Mahalanobis

predicted=classify(Mahalanobis,data_test[,-94])

str(predicted)

predicted <- predicted$pred_class

# Evaluate Model performance
#install.packages("e1071")
library(e1071)
library(caret)
confusionMatrix(predicted,
                data_test$target,
                mode="everything")


# Perform SVM

#library(e1071) 

#Radial Kernel
supvm = svm(target ~ ., data=data_train , kernel = "linear")

predicted = predict(supvm,newdata = data_test)

# Evaluate Model performance
install.packages("e1071")
library(e1071)
library(caret)
confusionMatrix(predicted,
                data_test$target,
                mode="everything")


