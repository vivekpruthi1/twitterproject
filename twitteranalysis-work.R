library(twitteR)
library(tm)
# let's find the locations 
locs<-availableTrendLocations()
locs
# see that the worldwide woeid is 1
#let's find united states woeid
usalocs<-locs[locs$country=="United States",]
nylocs<-usalocs[usalocs$name=="New York",]
nylocs
# Let's find what is trending in New York
NYTrends<-getTrends(woeid = nylocs$woeid)
NYTrends

#let's start the twitter sentiment analysis
# step1 : collecting tweets as corpus:
twitter_srch<-searchTwitter(searchString = "#trump",n = 5000,lang = "en")
# confirm the length
length(twitter_srch)
twitter_text<-sapply(twitter_srch,function(x) x$getText())
twitter_text
# step2 : clean the corpus
# we will design couple funtions
# first function is catch error
catch.error = function(x)
{
  # let us create a missing value for test purpose
  y = NA
  # Try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}
# second function is clean tweets

cleanTweets<- function(tweet){
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
# third function is to remove na's 

cleanTweetsAndRemoveNAs<- function(Tweets) {
  TweetsCleaned = sapply(Tweets, cleanTweets)
  # Remove the "NA" tweets from this tweet list
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  names(TweetsCleaned) = NULL
  # Remove the repetitive tweets from this tweet list
  TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned }

# step 3 : now we will clean the corpus using the functions:
twitter_text<-cleanTweetsAndRemoveNAs(twitter_text)
length(twitter_text)
# import the opinion lexicons
opinion.lexicon.pos<-scan('C:\\vik\\2017\\personal\\DSLA\\machine learning course\\week4\\positive-words.txt',
       what='character', comment.char=';')
opinion.lexicon.neg<-scan('C:\\vik\\2017\\personal\\DSLA\\machine learning course\\week4\\negative-words.txt',
                          what='character', comment.char=';')
