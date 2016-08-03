Work around data size constraints, borrowed a lot from http://varianceexplained.org/r/yelp-sentiment/

library(dplyr)
library(readr)
library(filehash)

blogs <- "~/Documents/sentiment_text/blogs.txt"
news <- "~/Documents/sentiment_text/news.txt"
twitter <- "~/Documents/sentiment_text/twitter.txt"

blogs <- read_lines(blogs)
news <- read_lines(news)
twitter <- readLines(twitter, warn = F)

# create platform
twitter3 <- transform(twitter, platform = "twitter")
blogs3 <- transform(blogs, platform = "blogs")
news3 <- transform(news, platform = "news")

# build db then start processing then split with id 
complete <- bind_rows(blogs3, news3, twitter3)
complete <- complete[c("platform", "X_data")]
id <- 1:nrow(complete)
complete <- cbind(id = id, complete)

blogs <- filter(complete, platform == "blogs")
news <- filter(complete, platform == "news")
twitter <- filter(complete, platform == "twitter")

# processing - tokenise and remove stopwords
library(tidytext)
tidy_blogs <- blogs %>%
  select(id, platform, X_data) %>%
  unnest_tokens(words, X_data)

data(stop_words)
tidy_blogs <- tidy_blogs %>%
  filter(!words %in% stop_words$word)

tidy_news <- news %>%
  select(id, platform, X_data) %>%
  unnest_tokens(words, X_data) %>%
  filter(!words %in% stop_words$word)

tidy_twitter <- twitter %>%
  select(id, platform, X_data) %>%
  unnest_tokens(words, X_data) %>%
  filter(!words %in% stop_words$word)

# use AFINN lexicon to score words
AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

# make sure same column names for join
colnames(tidy_blogs)[3] <- "word"
colnames(tidy_news)[3] <- "word"
colnames(tidy_twitter)[3] <- "word"

# for blogs
blogs_sentiment <- tidy_blogs %>%
  inner_join(AFINN, by = "word") %>%
  group_by(id)

blogs_av <- summarize(blogs_sentiment, sentiment = mean(afinn_score))

# for news
news_sentiment <- tidy_news %>%
  inner_join(AFINN, by= "word") %>%
  group_by(id)

news_av <- summarize(news_sentiment, sentiment = mean(afinn_score))

# for twitter
twitter_sentiment <- tidy_twitter %>%
  inner_join(AFINN, by = "word") %>%
  group_by(id)

twitter_av <- summarize(twitter_sentiment, sentiment = mean(afinn_score))
