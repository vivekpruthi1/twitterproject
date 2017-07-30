# 1.libraries
library(twitteR)
library(plyr)
library(stringr)
library(ggplot2)

# 2. creating the scoring function
#function score.sentiment

sentiment_score = function(sentences, pos.words, neg.words, .progress='none')
{  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

# 3. Imported positive and negative words

pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")

#4 . we have cleaned text from the tweets and other functions namely tw_trump_search_txt_cln1 and tw_POTUS_search_txt_cln1 .
# let see how many tweets from "trump" and " @POTUS"
no_of_tweets<-c(length(tw_trump_search_txt_cln1), length(tw_POTUS_search_txt_cln1))
names(no_of_tweets)<-c("trump","POTUS")
no_of_tweets

#5 join texts
trump_POTUS_alltweets = c(tw_trump_search_txt_cln1,tw_POTUS_search_txt_cln1)
length(trump_POTUS_alltweets)

#6 apply function sentiment_score
trump_POTUS_alltweets_score<-sentiment_score(trump_POTUS_alltweets,pos,neg,.progress = 'text')
head(trump_POTUS_alltweets_score)
nrow(trump_POTUS_alltweets_score)

#7 add variables to data frame trump_POTUS_alltweets_score that identifies from where tweet came - "trump" or "POTUS"
trump_POTUS_alltweets_score$source = factor(rep(c("trump", "POTUS"), no_of_tweets))
head(trump_POTUS_alltweets_score)
tail(trump_POTUS_alltweets_score)
table(trump_POTUS_alltweets_score$source,trump_POTUS_alltweets_score$score)
trump_POTUS_alltweets_score$very_positive = as.numeric(trump_POTUS_alltweets_score$score >= 4)
trump_POTUS_alltweets_score$positive = as.numeric(trump_POTUS_alltweets_score$score >= 0 & trump_POTUS_alltweets_score$score < 4)      
trump_POTUS_alltweets_score$neutral = as.numeric(trump_POTUS_alltweets_score$score ==0)
trump_POTUS_alltweets_score$negative = as.numeric(trump_POTUS_alltweets_score$score < 0 & trump_POTUS_alltweets_score$score > -4)
trump_POTUS_alltweets_score$very_negative = as.numeric(trump_POTUS_alltweets_score$score <= -4)
head(trump_POTUS_alltweets_score)

#8 how many very positives ,positive,neutral,negative and very negative

trump_POTUS_alltweets_emotion_verypos<-sum(trump_POTUS_alltweets_score$very_positive)->s1
trump_POTUS_alltweets_emotion_pos<-sum(trump_POTUS_alltweets_score$positive)->s2
trump_POTUS_alltweets_emotion_neutral<-sum(trump_POTUS_alltweets_score$neutral)->s3
trump_POTUS_alltweets_emotion_neg<-sum(trump_POTUS_alltweets_score$negative)->s4
trump_POTUS_alltweets_emotion_veryneg<-sum(trump_POTUS_alltweets_score$very_negative)->s5
global_pos_score<-(s1+s2)/(s1+s2+s3+s4+s5)
global_pos_score
str(trump_POTUS_alltweets_score)
#9 checking the spread among 2 feeds
# colors
cols <- c("#7CAE00","#C77CFF")
names(cols) <- c("trump", "POTUS")

# boxplot
ggplot(trump_POTUS_alltweets_score, aes(x=source, y=score, group=source)) +
  geom_boxplot(aes(fill=source)) +
  scale_fill_manual(values=cols) +
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3) +ggtitle(label ="Sentiment scores in twitter feeds from trump and POTUS")

#10 barplot of average score
trump_POTUS_alltweets_score_meanscores <- tapply(trump_POTUS_alltweets_score$score, trump_POTUS_alltweets_score$source, mean)
trump_POTUS_alltweets_score_meanscores_df <- data.frame(feed=names(trump_POTUS_alltweets_score_meanscores), meanscore=trump_POTUS_alltweets_score_meanscores)
trump_POTUS_alltweets_score_meanscores_df$feed<-reorder(trump_POTUS_alltweets_score_meanscores_df$feed,trump_POTUS_alltweets_score_meanscores_df$meanscore)

ggplot(trump_POTUS_alltweets_score_meanscores_df, aes(x=feed,y=meanscore)) +
  geom_bar(data=trump_POTUS_alltweets_score_meanscores_df, aes(x=feed, fill=feed),stat="identity") +
  scale_fill_manual(values=cols[order(trump_POTUS_alltweets_score_meanscores_df$meanscore)]) +
  ggtitle(label ="Average Sentiment Score")

# due to the plot it is neccessary to see why there is so much difference between the meanscore
meanscores_compare<-trump_POTUS_alltweets_score%>%group_by(source)%>%summarise(meanscore=mean(score),mean_verypos=mean(very_positive),mean_pos=mean(positive),mean_neutral=mean(neutral),mean_neg=mean(negative),mean_veryneg=mean(very_negative))
meanscores_compare<-as.data.frame(meanscores_compare)
sumscores_compare<-trump_POTUS_alltweets_score%>%group_by(source)%>%summarise(sumscore=sum(score),sum_vp=sum(very_positive),sum_p=sum(positive),sum_n=sum(neutral),sum_neg=sum(negative),sum_vn=sum(very_negative))
sumscores_compare<-as.data.frame(sumscores_compare)
library(tidyr)
library(gridExtra)
sumscore_compare_long<-gather(data = sumscores_compare,key = key,value = values,-source)
#following is the graph for the POTUS vs trump sum of emotions:
ggplot(sumscore_compare_long,aes(x=source,y=values))+geom_histogram(stat="identity",aes(fill=source))+facet_grid(~key)
p1<-sumscore_compare_long%>%filter(source=="trump")%>%ggplot(aes(x=key,y=values))+geom_histogram(stat="identity",aes(fill=key))+ggtitle(label = "categorised emotions from twitterfeeds with trump")
p2<-sumscore_compare_long%>%filter(source=="POTUS")%>%ggplot(aes(x=key,y=values))+geom_histogram(stat="identity",aes(fill=key))+ggtitle(label = "categorised emotions from twitterfeeds with POTUS")
grid.arrange(p1,p2,ncol=1,nrow=2)
###################################################################################################################
# timeseries analysis of negativity and positivity 
# we have 2 dataframes tweets
trump_dataframe<-tw_trump_search_df
POTUS_dataframe<-tw_POTUS_search_df
head(trump_dataframe)
str(trump_dataframe)
trump_dataframe<-trump_dataframe[,c(5,1)]
POTUS_dataframe<-POTUS_dataframe[,c(5,1)]
head(trump_dataframe)
trump_dataframe$source<-"trump"
POTUS_dataframe$source<-"POTUS"
trump_dataframe$created<-as_datetime(trump_dataframe$created)
POTUS_dataframe$created<-as_datetime(POTUS_dataframe$created)
