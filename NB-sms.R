# Read data
sms_raw <- read.csv(file.choose(), stringsAsFactors = F)
str(sms_raw)

# The sms type variable is a character vector. 
# Since this is categorical, 
# it would be better to convert it into factor variable
sms_raw$type <- as.factor(sms_raw$type)

table(sms_raw$type)
prop.table(table(sms_raw$type))

# SMS messages are strings of text composed of words, spaces, 
# numbers and punctuations
# One needs to know how to remove numbers, punctuations
# and handle uninteresting words like and, but and or;
# and how to break apart sentences into individual words
# tm package can help in this
library(tm)

# The first step in processing text data involved creating a corpus, 
# which is a collection of text documents
# In order to create a corpous we will use VCorpus() function in the tm package
# VCorpus() refers to volatile as it is stored in memory as opposed to 
# being stored in the disk
# PCorpus() function can be used to access permanent corpus stored in the 
# database
# Since we have already loaded the SMS messages texts into R, we will use 
# the VectorSource() 
# reader function
# to create a source object from the existing sms_raw$text vector which can then 
# be supplied to VCorpus()

sms_corpus <- VCorpus(VectorSource(sms_raw$text))

# By printing the corpus, we see that it contains documents for each of the 5559 
# SMS messages in the training data
print(sms_corpus)

# To view the actual message text, you can use the as.character() function
as.character(sms_corpus[[1]])
as.character(sms_corpus[[4]])

# To view multiple documents, we need to use as.character() on several 
# items in the corpous
# To do so, we will use lapply() function
lapply(sms_corpus[1:5], as.character)

# The corpus contains 5559 text messages
# In order to perform our analysis, we will need to divide 
# these messages into individual words
# But first we need to clean the texts, in order to standardize the words, 
# by removing punctuations & other unwanted characters
# The tm_map() function provides a method to apply a 
# transformation to the corpus



# We start with standardizing the messages to use only lower case
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

# Check if the tolower() function worked
as.character(sms_corpus[[4]])
as.character(sms_corpus_clean[[4]])

# lets remove the numbers from the SMS messages
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
as.character(sms_corpus[[3]])
as.character(sms_corpus_clean[[3]])

# Our next task is to remove filler words such as to, and, 
# but and or from our SMS messages
# These terms are known as STOP WORDS and are typically removed prior 
# to text mining
# We use the stopwords() function and removeWords to remove the STOP WORDS
stopwords()
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())

mystopwords <- c("ibiza", "tscs", "www")
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, mystopwords)
as.character(sms_corpus[[4]])
as.character(sms_corpus_clean[[4]])

# Next step to remove the punctuations
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
as.character(sms_corpus[[4]])
as.character(sms_corpus_clean[[4]])

# Another common standardization for text data involves 
# reducing words to their root 
# form in a process called STEMMING. For example words like learned, learning, 
# learnt get transformed to learn
# The tm package provides STEMMING functionality via integration 
# with SnowballC package
library("SnowballC")
# See how it works
wordStem(c("learning", "learn", "learns", "learned"))
wordStem(c("work", "working", "worked"))
# but.....
wordStem(c("go", "going", "gone", "went"))

# Now applying STEMMING to our courpus
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

# After removing numbers, stop words, punctuations and performing stemming, 
# now we remove whitespaces
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)


# Visualizing text data - Word Clouds
library("wordcloud")
wordcloud(sms_corpus_clean, min.freq=50, random.order=TRUE)
# This will create a word cloud from our SMS corpus
# Since we have specified random.order=FALSE, the cloud will be arranged
# in a nonrandom order with higher frequency words placed to the center
# The min.freq parameter specifies the number of times a word must appear in the 
# corpus before it will be displayed in the cloud

# Let us now compare the visualization between SPAM and HAM emails
spam <- subset(sms_raw, type=="spam")
ham <- subset(sms_raw, type=="ham")
par(mfcol=c(1,2))
wordcloud(spam$text, min.freq=50, random.order=FALSE, scale=c(3,0.5))
wordcloud(ham$text, min.freq=50, random.order=FALSE, scale=c(3,0.5))
# The scale parameter allows us to adjust the maximum and minimum font size for 
# words in the cloud


# Splitting text documents into words
# Now that data is processed, the final step is to split 
# the messages into individual
# components through a process called TOKENIZATION
# A token is a single element of a text string

# The DocumentTermMatrix() function in the tm package will take a corpus and create 
# a data structure called 
# Document Term Matrix (DTM) in which rows indicate documents (SMS Messages) and 
# columns indicate words
# Creating a DTM sparse matrix, given a tm corpus:
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
dim(sms_dtm)
inspect(sms_dtm)

# DATA PREPARATION - we will split into train & test data
sms_dtm_train <- sms_dtm_after_ngram[1:4169,]
sms_dtm_test <- sms_dtm_after_ngram[4170:5556,]

# To confirm that the subsets are representative of the 
# complete set of SMS data
sms_train_labels <- sms_raw[1:4169,]$type
sms_test_labels <- sms_raw[4170:5556,]$type
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# We notice that both the train & test data contain about 13% spam. 
# This suggests 
# that the spam messages
# were divided evenly between the 2 datasets - train & test

# THE FINAL STEP is to transform the sparse matrix into a data structure that 
# Naive Bayesian Classifier can consume

# Currently the sparse matrix includes over 6500 features - 
# this is a feature for every word
# Its unlikely that all of these are useful for classification
# So we remove those features/words which appear in less than 5 messages

# We can find the frequent words using findFreqTerms() function in the 
# tm package
sms_frequent_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_frequent_words)

# Filter out the non frequent words from the training & test dataset
sms_dtm_freq_train <- sms_dtm_train[, sms_frequent_words]
sms_dtm_freq_test <- sms_dtm_test[, sms_frequent_words]

# Finally - because Naive Bayesian is typically trained on data with CATEGORICAL 
# FEATURES
# The sparse matrix poses a problem because the cells in the sparse matrix are 
# numeric and measure the number of 
# times a word appears in a message
# We need to change this to a categorical variable that simply indicates yes/no 
# depending on whether the
# word appears at all 

# We write a function to convert counts to Yes/No strings:
convert_counts <- function(x){
  x <- ifelse(x>0, "Yes", "No")
}
# We need to apply convert_counts() to each of the columns in 
# the sparse matrix
# The apply() function allows a function to be used on each of 
# the rows & columns in a matrix
# It uses MARGIN parameter to specify either rows or columns. 
# MARGIN=1 means rows, 
# MARGIN=2 means columns
# We are interested in columns
options(max.print = 1000000)
sms_train <- apply(sms_dtm_freq_train, MARGIN=2, convert_counts)
sms_train
sms_test <- apply(sms_dtm_freq_test, MARGIN=2, convert_counts)
sms_test
# Result will be 2 character type matrixes, 
# each with cells indicating "Yes" or "No" 
# for whether the word represented
# by the column appears at any point in the message represented by the row
# TRAINING THE MODEL
library("e1071")

# To build our model on the sms_train:
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

# EVALUATING MODEL PERFORMANCE
# To evaluate the SMS classifier, we need to test its prediction on unseen messages
# in the test data
# Recall that unseen message features are in a matrix named sms_test
# While the class labels (spam or ham) are stored in a vector names sms_test_labels
# Model we built for classification is named sms_classifier
# We use predict() function to make predictions
sms_test_pred <- predict(sms_classifier, sms_test)

# To compare the predictions to the true values, we will use CrossTable() function in the 
# gmodels package
install.packages("gmodels")
library("gmodels")
CrossTable(sms_test_pred, sms_test_labels, prop.chisq = FALSE, 
           prop.r = FALSE, prop.c=FALSE, dnn=c('predicted', 'actual'))

# Looking at the table, we see that a total of just 6+29=35 out of 1390 
# SMS messages were 
# incorrectly classified
# thats 2.6%

# Given the little effort we have put, this type of result is quite impressive. 
# This exemplifies why Naive Bayesian is the standard for text classification

# ON THE OTHER HAND, the 6 legitimate messages that were incorrectky 
# classified as spam 
# could cause significant problems for the deployment of our filtering algo
# because the filter could cause a person to miss an important text message
# We should investigate to see whether we can slightly tweak the 
# model to archeve better 
# performance

# IMPROVING MODEL PERFORMANCE
# Read LAPLACE ESTIMATOR
# Just because the word "ringtone" only appeared in the spam messages in 
# the training data, 
# it does not
# mean that every message with this word should be classified as spam

# We will rebuild the model with LAPLACE estimator=1
sms_classifier_laplace <- naiveBayes(sms_train, sms_train_labels, laplace=1)
# Now predict
system.time(sms_test_pred_laplace <- predict(sms_classifier_laplace, sms_test))

CrossTable(sms_test_pred_laplace, sms_test_labels, prop.chisq = FALSE, 
           prop.r=FALSE, prop.c=FALSE, dnn=c('predicted', 'actual'))

# Adding the laplace estimator, the number of false positives (ham messages wrongly 
# classified as spam) is reduced from 6 to 5
# and the number of false negatives (spam wrongly classified as ham) from 30 to 28
# VERY IMPORTANT NOTE: The model prior to LAPLACE estimator was good enough. 
# However we got better result with 
# LAPLACE estimator. We should be careful in tweaking too much as we need to have a 
# balance maintained being overly
# agressive & overly passive while filtering spam
# Users would prefer that a small number of spam messages slip through the 
# filter than an alternative
# in which ham messages are filtered too aggressively

