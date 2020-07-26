Sys.unsetenv("http_proxy")
Sys.unsetenv("https_proxy")


sms_raw <- read.csv(file.choose(), stringsAsFactors = F)

str(sms_raw)

sms_raw$type <- as.factor(sms_raw$type)

table(sms_raw$type)

prop.table(table(sms_raw$type))

install.packages("tm")
library(tm)

#VCorpus()

#PCorpus()

sms_corpus <- VCorpus(VectorSource(sms_raw$text))

print(sms_corpus)

as.character(sms_corpus[[2]])

sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

as.character(sms_corpus[[3]])
as.character(sms_corpus_clean[[2]])

sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

as.character(sms_corpus[[3]])
as.character(sms_corpus_clean[[3]])

stopwords()

sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())

mystopwords <- c("ibiza", "tscs", "www", "hor")

sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, mystopwords)


as.character(sms_corpus[[4]])
as.character(sms_corpus_clean[[4]])

sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

as.character(sms_corpus[[4]])
as.character(sms_corpus_clean[[4]])

library("SnowballC")

wordStem(c("learning", "learn", "learns", "learned"))

wordStem(c("go", "going", "gone", "went"))


sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

print(sms_corpus_clean)


##################################################################



library("wordcloud")

wordcloud(sms_corpus_clean, min.freq=50, random.order=TRUE)

spam <- subset(sms_raw, type=="spam")
ham <- subset(sms_raw, type=="ham")

wordcloud(spam$text, min.freq=50, random.order=FALSE, scale=c(3,0.5))
wordcloud(ham$text, min.freq=50, random.order=FALSE, scale=c(3,0.5))

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
dim(sms_dtm)

inspect(sms_dtm_raw)

# For raw data

sms_dtm_raw <- DocumentTermMatrix(sms_corpus)
dim(sms_dtm)

inspect(sms_dtm)


sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5556,]

sms_train_labels <- sms_raw[1:4169,]$type
sms_test_labels <- sms_raw[4170:5556,]$type

prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

sms_frequent_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_frequent_words)


sms_dtm_freq_train <- sms_dtm_train[, sms_frequent_words]
sms_dtm_freq_test <- sms_dtm_test[, sms_frequent_words]

convert_counts <- function(x){
  x <- ifelse(x>0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN=2, convert_counts)


sms_test <- apply(sms_dtm_freq_test, MARGIN=2, convert_counts)


library("e1071")

sms_classifier <- naiveBayes(sms_train, sms_train_labels)

sms_test_pred <- predict(sms_classifier, sms_test)

sms_test_pred

sms_classifier

library(caret)

?confusionMatrix

sms_test

cm_test = confusionMatrix(sms_test_labels, sms_test_pred)

cm_test


#F1 <- (2 * precision * recall) / (precision + recall)

F1_Test <-  (2 * cm_test$table * recall) / (precision + recall)

cm_test

sms_train_pred <- predict(sms_classifier, sms_train)

sms_train_labels
cm_train = confusionMatrix(sms_train_labels, sms_train_pred)

cm_test$byClass['F1']


Testresult <- confusionMatrix(sms_test_pred, sms_test_labels, mode="everything")
Testresult

Trainresult <- confusionMatrix(sms_train_pred, sms_train_labels, mode="everything")
Trainresult


cm_train$positive

#########################################################################
### Better printing Confusion Matrix #####


install.packages("gmodels")
library("gmodels")
CrossTable(sms_test_pred, sms_test_labels, prop.chisq = FALSE, 
           prop.r = FALSE, prop.c=FALSE, dnn=c('predicted', 'actual'))



######################################################################