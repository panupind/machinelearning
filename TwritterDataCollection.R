Sys.setenv(http_proxy = "")
Sys.getenv("http_proxy")

Sys.setenv(https_proxy = "")
Sys.getenv("https_proxy")


install.packages("bit64")
install.packages("twitteR")
install.packages("ROAuth")
library("twitteR")
library("ROAuth")

#Set the working directory
#setwd("~/ML/bangaloresession1")
getwd()

api_key <- "OmdK90LrAmxRgJUqe3Jw2lbOq"
api_secret <- "scFt16snW9QgfO2sPN8yR1AInlEAxj02zR2YtSEozUQOZ6vI1r"
access_token <- "949315530938200064-4b1GIro58LU4nWPefmnIrgDh3wkmB0K"
access_token_secret <- "SntWz2t5rwpi4OOkniUN0A8CjE1OPDobrGx49adjdNAi9"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


xiaomi_tweets <- userTimeline("coronavirus", n = 2000)
xiaomi_tweets_df <- twListToDF(xiaomi_tweets)
dim(xiaomi_tweets_df)
View(xiaomi_tweets_df)

#write.csv(xiaomi_tweets_df,file=paste("XiaomiIndiaTweets.csv"))
write.csv(xiaomi_tweets_df,file=paste("coronavirus.csv"))

xiaomi_tweets_df<-read.csv("coronavirus.csv",stringsAsFactors = FALSE)

View(xiaomi_tweets_df)
