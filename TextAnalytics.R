install.packages("qdap")
library(qdap)

text = "The Gray Matters team has the the the the the the the the the the the  always been willing to co-learn with us and has shown great flexibility in evolving with the Ed Alliance as our data needs change. The insights from their assessments have been critical to managing our partner base and the quantifiable evidence they provide on learning outcomes is core to Ed Alliance’s theory of change"

my_freq_terms = freq_terms(text,10)
plot(my_freq_terms)


tweets = read.csv("XiaomiIndiaTweets.csv", stringsAsFactors =  FALSE)
str(tweets)
nrow(tweets)

xiaomi_tweets <- tweets$text
xiaomi_tweets

str(xiaomi_tweets)
xiaomi_tweets[15]

stopwords("en")


install.packages("tm")
library(tm)

xiaomi_source = VectorSource(xiaomi_tweets)
xiaomi_corpus = VCorpus(xiaomi_source)

xiaomi_corpus
str(xiaomi_corpus)

xiaomi_corpus[[1]]

xiaomi_corpus[[15]][1]

removeWords(xiaomi_tweets,stopwords("en"))

new_stops <- c("xiaomi", "redmi" , stopwords("en"))

data = removeWords(xiaomi_tweets, new_stops)

data

#xiaomi_tweets[15]
