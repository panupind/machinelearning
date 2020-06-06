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
