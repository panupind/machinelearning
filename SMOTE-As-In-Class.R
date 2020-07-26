#creditcard.csv
#Synthetic minority over-sampling technique.

x <- read.csv(file.choose())

set.seed(1029)
## Remove rows that do not have target variable values
final <- x[!(is.na(x$Class)),]

final$Class <- factor(final$Class)

library(caTools)

split <- sample.split(final$Class, SplitRatio = 0.75)

dresstrain <- subset(final, split == TRUE)
dresstest <- subset(final, split == FALSE)


## Let's check the count of unique value in the target variable
as.data.frame(table(dresstrain$Class))



## Loading DMwr to balance the unbalanced class
install.packages("DMwR")
library(DMwR)

## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
balanced.data <- SMOTE(Class ~., dresstrain, perc.over = 4800, k = 5, perc.under = 1000)

as.data.frame(table(balanced.data$Class))

# logic for perc.over is (48*369 + 369) = 18081
# logic for perc.under is (10*48*369) = 177120



# Build Logistic Regression Model

library(caret)  

model <- glm (Class~., data=balanced.data, family = binomial)
summary(model)

## Predict the Values
predict <- predict(model, dresstest, type = 'response')

## Create Confusion Matrix
table(dresstest$Class, predict > 0.5)

#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, dresstest$Class)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE)


# Random Forest
library(randomForest)  
library(e1071)  


rf = randomForest(Class~.,  
                  ntree = 100,
                  data = balanced.data)
plot(rf) 

#Variable Importance
varImp(rf)

varImpPlot(rf,  
           sort = T,
           n.var=25,
           main="Variable Importance")


predicted.response <- predict(rf, dresstest)


confusionMatrix(data=predicted.response,  
                reference=dresstest$Class)


# 1 - Verify performance without balancing

# 2 - Can you solve this problem of imbalance using threshold criteria?

# 3 - Will ensemble methods solve the problem?

# 4 - Sampling methods

# 4.1 - Calculate 3-4 buckets of class balances

# Create instances of the train data for all conditions

# model evaluation - K fold cross validation, LOOCV(small), (most time consuming)Repeated k-fold cross validation

# train function in caret

