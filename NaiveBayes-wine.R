install.packages("naivebayes")
library(naivebayes)

data <- read.csv(file.choose())

str(data)

data_train <- data[1:145,]
data_test <- data[146:177,-1]

nb <- naive_bayes(Quality ~ ., data_train)

data_pred <- predict(nb,data_test,type="class")

confusionMatrix(data_pred,data$Quality[146:177], positive = "a", mode="everything")



predict(nb,data_test,type="prob")
