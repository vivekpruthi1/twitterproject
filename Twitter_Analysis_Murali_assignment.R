
# Install packages
doInstall <- FALSE  # Change to FALSE if you don't want packages installed.
loadTweets <- FALSE # Change to TRUE if you want to download tweets from twitter


if(doInstall){
  install.packages("twitteR")
  install.packages("tidyverse")
  install.packages("lubridate")
  install.packages("stringr")
  install.packages("tidytext")
  install.packages("broom")
  install.packages("scales")
  install.packages("wordcloud")
  install.packages("reshape2")
  
  #install.packages("rgra")
}

# Load libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(tidytext)
library(broom)
library(scales)
library(twitteR)
library(wordcloud)
library(reshape2)

ConsumerKey = "5N6OrTxIT6cAxgZIYIzCYM0UW"
ConsumerSecret = "gTmbQ0gnFPQNPXcQC9Z7rU0NH0Mm23YYkMTEeA78fU0Ciad2ko"
AccessToken = "50372718-VbWy8VaVx8TwaaumYcJZtBc3M7jtKP9oRAud0xTgj"
AccessTokenSecret = "vPiIhiaBkDxNgKkcmpApVU1tXh8629eWdlhpokDxLFzJc"

if (loadTweets) {
  setup_twitter_oauth(ConsumerKey, ConsumerSecret, AccessToken, AccessTokenSecret)
  
  # Search Twitter (returns 25 tweets by default)
  #tweet_raw <- searchTwitter(searchString = "#DataScience",n = 3000)
  
  tweet_raw <- userTimeline('realdonaldtrump',n = 3200, includeRts=TRUE)
  
  tweets<- strip_retweets(tweet_raw, strip_manual=TRUE, strip_mt=TRUE)
  head(tweets,5)
  
  # Convert the list of tweets into a data frame
  tweetDF<-twListToDF(tweets)
  
  if(nrow(tweetDF) > 0) {
    write.csv(tweetDF, "./tweet_trump_raw")
  }
  
}  else {
  tweetDF <- read.csv("./tweet_trump_raw")
}
```

```{r}
head(tweetDF)
```

```{r}
tweetDF$created <- as_datetime(tweetDF$created)
tweetDF<-as.tbl(tweetDF)
tweetDF
```

#Extract (from statusSource) the name of the application used to generate the Tweet
```{r}
tweets <- tweetDF %>%
  select(id, statusSource, text, created) %>%
  extract(statusSource, "source", "Twitter for (.*?)<") %>%
  filter(source %in% c("iPhone", "Android"))

tweets %>%
  head() %>%
  knitr::kable(caption = "Example of Donald Trump tweets")


#Compare time of day that tweets were sent from android and iPhone
tweets %>%
  count(source, hour = hour(with_tz(created, "EST"))) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")


#Trump manually retweets by copying other tweets and surrounds them with ""
tweets %>%
  count(source,
        quoted = ifelse(str_detect(text, '^"'), "Quoted", "Not quoted")) %>%
  ggplot(aes(source, n, fill = quoted)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "") +
  ggtitle('Whether tweets start with a quotation mark (")')


##Analyze the words now
library(tidytext)

# custom regular expression to tokenize tweets
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"   

# function to neatly print the first 10 rows using kable
print_neat <- function(df){
  df %>%
    head() %>%
    knitr::kable()
}


# tweets data frame
tweets %>%
  print_neat()

# remove manual retweets
tweets %>%
  filter(!str_detect(text, '^"')) %>%
  print_neat()
# remove urls
tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  print_neat()

# unnest into tokens - tidytext format
tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  print_neat()

# remove stop words
# tweets %>%
#   filter(!str_detect(text, '^"')) %>%
#   mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
#   unnest_tokens(word, text, token = "regex", pattern = reg) %>%
#   filter(!word %in% stop_words$word,
#          str_detect(word, "[a-z]")) %>%
#   print_neat()

# remove stop words
tweet_words <- tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

#Add all the tweet words from iPhone and Android together
tweet_words_count <- tweet_words %>%
  count(source, word, sort = TRUE) %>%
  ungroup()
tweet_words_count


#Group words by iPhone and android again
#adding totals column
total_words <- tweet_words_count %>%
  group_by(source) %>%
  summarize(total = sum(n))
total_words

tweet_words_count <- left_join(tweet_words_count, total_words)
tweet_words_count

#Add term frequency(tf) and inverse document frequency(idf)
tweet_words_count <- tweet_words_count %>%
  bind_tf_idf(word, source, n)
tweet_words_count

#Which terms have high tf-idf?
tweet_words_count %>%
  select(-total) %>%
  arrange(desc(tf_idf))

tweet_important <- tweet_words_count %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

#Graph Highest freq words by source
tweet_important %>%
  group_by(source) %>%
  slice(1:15) %>%
  ggplot(aes(word, tf_idf, fill = source)) +
  geom_bar(alpha = 0.8, stat = "identity") +
  labs(title = "Highest tf-idf words in @realDonaldTrump",
       subtitle = "Top 15 for Android and iPhone",
       x = NULL, y = "tf-idf") +
  coord_flip()

### Sentiment analysis
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  select(word, sentiment)
nrc

#To measure the sentiment of the Android and iPhone tweets, 
#count the number of words in each category
sources <- tweet_words %>%
  group_by(source) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(id, source, total_words)
sources

by_source_sentiment <- tweet_words %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, id) %>%
  ungroup() %>%
  complete(sentiment, id, fill = list(n = 0)) %>%
  inner_join(sources) %>%
  group_by(source, sentiment, total_words) %>%
  summarize(words = sum(n)) %>%
  ungroup()

head(by_source_sentiment)

# function to calculate the poisson.test for a given sentiment
poisson_test <- function(df){
  poisson.test(df$words, df$total_words)
}

# use the nest() and map() functions to apply poisson_test to each sentiment and
# extract results using broom::tidy()
sentiment_differences <- by_source_sentiment %>%
  group_by(sentiment) %>%
  nest() %>%
  mutate(poisson = map(data, poisson_test),
         poisson_tidy = map(poisson, tidy)) %>%
  unnest(poisson_tidy, .drop = TRUE)
sentiment_differences

#visualize it with a 95% confidence interval
sentiment_differences %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, estimate)) %>%
  mutate_each(funs(. - 1), estimate, conf.low, conf.high) %>%
  ggplot(aes(estimate, sentiment)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  scale_x_continuous(labels = percent_format()) +
  labs(x = "% increase in Android relative to iPhone",
       y = "Sentiment")


#which words drove this difference in sentiment. 
#Let’s consider the words with the largest changes within each category
tweet_important %>%
  inner_join(nrc, by = "word") %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  mutate(sentiment = reorder(sentiment, -tf_idf),
         word = reorder(word, -tf_idf)) %>%
  group_by(sentiment) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = source)) +
  facet_wrap(~ sentiment, scales = "free", nrow = 4) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "",
       y = "tf-idf") +
  scale_fill_manual(name = "", labels = c("Android", "iPhone"),
                    values = c("red", "lightblue"))

#Word cloud
library(wordcloud)

tweet_words %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, scale = c(3,.5)))

###Compare Trump's tweets to Hillary's
library(reshape2)

# get tweets
hillary <- userTimeline("hillaryclinton", n = 3200) %>%
  twListToDF %>%
  as_tibble

# tokenize
hillary_token <- hillary %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

#Convert Id to numeric
hillary_token$id = as.numeric(hillary_token$id)


# Compare Trump and Hillary word clouds
bind_rows(Trump = tweet_words, Hillary = hillary_token, .id = "person") %>%
  count(word, person) %>%
  acast(word ~ person, value.var = "n", fill = 0) %>%
  comparison.cloud(max.words = 100, colors = c("blue", "red"))


