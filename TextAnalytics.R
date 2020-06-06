install.packages("qdap")
library(qdap)
install.packages("SnowballC")
library(SnowballC)

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

# Display the VCorpus
xiaomi_corpus[[1]]
xiaomi_corpus[[15]][1]


#Removing the stop words
removeWords(xiaomi_tweets,stopwords("en"))
#Adding new stop words
new_stops <- c("xiaomi", "redmi" , stopwords("en"))
data = removeWords(xiaomi_tweets, new_stops)

# Display Data once all stop words ( Std  + New ) are removed
data

#xiaomi_tweets[15]

# Clean Corpus - Doing more cleaning , and punctuation replacement 

clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  #corpus <- tm_map(corpus, content_transformer(replace_))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c("xiaomi", "redmi", stopwords("en")))
  return(corpus)
}

clean_corp<-clean_corpus(xiaomi_corpus)
clean_corp[[227]][1]
tweets$text[227]

#Create TDM and DTM

xiaomi_tdm<-TermDocumentMatrix(clean_corp)
xiaomi_dtm<-DocumentTermMatrix(clean_corp)

# Deal with DTM First, convert as Matrix

xiaomi_m<-as.matrix(xiaomi_dtm)
dim(xiaomi_m)
xiaomi_m[100:104, 1:5]

# Deal with TDM , convert as Matrix

xiaomi_t<-as.matrix(xiaomi_tdm)
xiaomi_t[100:106,100:104]
xiaomi_m<-as.matrix(xiaomi_tdm)

# count the frequency of words from TDM

term_frequency<-rowSums(xiaomi_m)
term_frequency<-sort(term_frequency,decreasing = TRUE)
term_frequency[1:10]
barplot(term_frequency[1:10],col="tan",las=2)


# Count frequency of terms using qdap as well
library(qdap)
frequency<-freq_terms(tweets$text,top=10,at.least=3,
                      stopwords="Top200Words")
plot(frequency)
frequency2<-freq_terms(tweets$text,top=10,at.least=3,
                       stopwords = tm::stopwords("english"))
plot(frequency2)


# Remove sparse terms from TDM

dim(xiaomi_tdm)
tdm1<-removeSparseTerms(xiaomi_tdm,sparse=0.95)
tdm1
tdm2<-removeSparseTerms(xiaomi_tdm,sparse = 0.975)
tdm2
tdm_m<-as.matrix(tdm2)
tdm_df<-as.data.frame(tdm_m)

# Prepare and create clustering

tweets_dist<-dist(tdm_df)
hc<-hclust(tweets_dist)
plot(hc)


library(dendextend)
hcd<-as.dendrogram(hc)
hcd<-branches_attr_by_labels(hcd,c("eduaubdedubu",
                                   "eduaubdedubu",
                                   "merepaasgifhai"),"red")
plot(hcd,main="better dendogram")
rect.dendrogram(hcd,k=2,border = "red")
rect.dendrogram(hcd,k=3,border = "green")


# Word Associations

associations<-findAssocs(xiaomi_tdm,"fans",0.2)
associations
summary(associations)
list_vect2df(associations)
associations_df <- list_vect2df(associations)[, 2:3]
library(ggplot2)
ggplot(associations_df, aes(y = associations_df[, 1])) +   geom_point(aes(x = associations_df[, 2]),              data = associations_df, size = 3) 






?qdap::bracketX()
