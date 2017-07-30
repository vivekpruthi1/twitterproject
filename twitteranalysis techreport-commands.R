# load packages
library(twitteR)
library(tm)
library(tidytext)
library(ggplot2)
library(stringi)
library(stringr)
library(wordcloud)
library(rebus)
# connection to twitter
source("C:\\Users\\vivek\\Documents\\dsla1\\twitterproject\\07.28.2017\\connectiontwitter.R")
# user info profile
tw_user_potus<-getUser(user = "POTUS")
# check the profile structure
class(tw_user_potus)
str(tw_user_potus)
# check other data available
tw_user_potus$description
tw_user_potus$getStatusesCount()
tw_user_potus$getFollowersCount()
tw_user_potus$getFriendsCount()
tw_user_potus$getCreated()
tw_user_potus$favoritesCount
tw_user_potus$lastStatus
tw_user_potus$lastStatus$retweetCount
tw_user_potus$getListedCount()
# get the tweets
tw_trump_search <- searchTwitter(searchString = "trump",n = 5000,lang = "en")
length(tw_trump_search)
head(tw_trump_search)
tw_POTUS_search <- searchTwitter(searchString = "@potus",n = 5000,lang = "en")
length(tw_POTUS_search)
head(tw_POTUS_search)
tail(tw_POTUS_search)
# strip the retweets
tw_trump_search<-strip_retweets(tw_trump_search, strip_manual=TRUE, strip_mt=TRUE)
tw_POTUS_search<-strip_retweets(tw_POTUS_search, strip_manual=TRUE, strip_mt=TRUE)
length(tw_trump_search)
length(tw_POTUS_search)
# list to dataframe
tw_POTUS_search_df<-twListToDF(tw_POTUS_search)
head(tw_POTUS_search_df)
str(tw_POTUS_search_df)
tw_trump_search_df<-twListToDF(tw_trump_search)
head(tw_trump_search_df)
str(tw_trump_search_df)
# clean function define
cleantweets<- function(tweet){
  # Clean the tweet for sentiment analysis
  # remove html links, which are not required for sentiment analysis
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  # First we will remove retweet entities from  the stored tweets (text)
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Then remove all "#Hashtag"
  tweet = gsub("#\\w+", " ", tweet)
  # Then remove all "@people"
  tweet = gsub("@\\w+", " ", tweet)
  # Then remove all the punctuation
  tweet = gsub("[[:punct:]]", " ", tweet)
  # Then remove numbers, we need only text for analytics
  tweet = gsub("[[:digit:]]", " ", tweet)
  # finally, we remove unnecessary spaces (white spaces, tabs etc)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
}
# clean function 2 define
clean_tweets_and_na<- function(Tweets) {
  TweetsCleaned = sapply(Tweets, cleantweets)
  # Remove the "NA" tweets from this tweet list
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  names(TweetsCleaned) = NULL
  # Remove the repetitive tweets from this tweet list
  TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned }
# get text
tw_trump_search_txt<-sapply(tw_trump_search,function(x) x$getText())
tw_POTUS_search_txt<-sapply(tw_POTUS_search,function(x) x$getText())
# to remove emoji/icons for mac use "UTF-8-MAC"
tw_trump_search_txt<-sapply(tw_trump_search_txt,function(x) iconv(x ,to="UTF-8",sub = "" ))
tw_POTUS_search_txt<-sapply(tw_POTUS_search_txt,function(x) iconv(x ,to="UTF-8",sub = "" ))
# clean text with functions
tw_trump_search_txt_cln1<-clean_tweets_and_na(tw_trump_search_txt)
tw_POTUS_search_txt_cln1<-clean_tweets_and_na(tw_POTUS_search_txt)
# create corpus
tw_trump_search_corpus<-Corpus(VectorSource(tw_trump_search_txt_cln1))
tw_POTUS_search_corpus<-Corpus(VectorSource(tw_POTUS_search_txt_cln1))
# get stopwords
tw_stopwords <-stopwords("english")
# corpus cleaning function
cleancorpus<-function(incorpus){
  incorpus <- tm_map(incorpus, tolower)
  incorpus <- tm_map(incorpus, removePunctuation)
  incorpus <- tm_map(incorpus, removeNumbers)
  incorpus <- tm_map(incorpus, removeWords, tw_stopwords)
}
# clean corpus
tw_trump_search_corpus<-cleancorpus(tw_trump_search_corpus)
tw_POTUS_search_corpus<-cleancorpus(tw_POTUS_search_corpus)
inspect(tw_trump_search_corpus)
inspect(tw_POTUS_search_corpus)
# create document matrix
tw_trump_DTM<-DocumentTermMatrix(tw_trump_search_corpus,list(termFreq=1))
inspect(tw_trump_DTM)
saveRDS(tw_trump_DTM,"./tw_trump_DTM")
tw_POTUS_DTM<-DocumentTermMatrix(tw_POTUS_search_corpus,list(termFreq=1))
inspect(tw_POTUS_DTM)
saveRDS(tw_POTUS_DTM,"./tw_POTUS_DTM")
# find frequent terms
tw_trump_freqterms<-findFreqTerms(tw_trump_DTM,lowfreq = 10)
tw_POTUS_freqterms<-findFreqTerms(tw_POTUS_DTM,lowfreq = 10)
tw_trump_freqterms
tw_POTUS_freqterms
# find frequencies
tw_trump_termfreq<-colSums(as.matrix(tw_trump_DTM))
tw_trump_termfreq_df<-data.frame(term = names(tw_trump_termfreq),freq=tw_trump_termfreq)
tw_POTUS_termfreq<-colSums(as.matrix(tw_POTUS_DTM))
tw_POTUS_termfreq_df<-data.frame(term = names(tw_POTUS_termfreq),freq=tw_POTUS_termfreq)

# Word Cloud for the keyword "trump"
wordcloud(words = tw_trump_freqterms,
          freq = tw_trump_termfreq_df[tw_trump_termfreq_df$term %in% tw_trump_freqterms,2],
            colors = T,random.color = T,scale = c(10,0.5))
# Word Cloud for the user "POTUS
wordcloud(words = tw_POTUS_freqterms,
          freq = tw_POTUS_termfreq_df[tw_POTUS_termfreq_df$term %in% tw_POTUS_freqterms,2],
          colors = T,random.color = T,scale = c(10,0.5))
#comparison cloud

tweets_trump<-paste(tw_trump_search_txt_cln1, collapse=" ")
tweets_POTUS<-paste(tw_POTUS_search_txt_cln1, collapse=" ")
total<-c(tweets_trump,tweets_POTUS)
total_corpus<-Corpus(x = VectorSource(total))
total_tdm<-TermDocumentMatrix(total_corpus)
total_tdm_matrix<-as.matrix(total_tdm)
total_tdm_matrix
colnames(total_tdm_matrix)<-c("trump","POTUS")
comparison.cloud(total_tdm_matrix,random.order = FALSE,colors = c("red","blue"),title.size = 1.5,max.words = 500)

#commonality cloud
commonality.cloud(total_tdm_matrix,max.words = 500,title.size=1.5)


