## Reference1: https://www.tidytextmining.com/sentiment.html
## Reference2: https://medium.com/mindninja/sentiment-analysis-on-twitter-data-with-r-e93769feb8c4

library(dplyr)
library(tidytext)
library(ggplot2)

### Data cleaning process ###
## removing any http links #
gfloyd2$clean_text <-gsub("http.*","", gfloyd2$tweet)
gfloyd2$clean_text <-gsub("https.*","", gfloyd2$clean_text)

## removing certain words ###
gfloyd2$clean_text <-gsub("pic.twitter.com","",gfloyd2$clean_text)
gfloyd2$clean_text <-gsub("george","",gfloyd2$clean_text)
gfloyd2$clean_text <-gsub("floyd","",gfloyd2$clean_text)
gfloyd2$clean_text <-gsub("georgefloyd","",gfloyd2$clean_text)
gfloyd2$clean_text <-gsub("icantbreathe","",gfloyd2$clean_text)
gfloyd2$clean_text <-gsub("blacklivesmatter","",gfloyd2$clean_text)
gfloyd2$clean_text <-gsub("chauvin","",gfloyd2$clean_text)
gfloyd2$clean_text <-gsub("derek","",gfloyd2$clean_text)

## removing puncuations, lowercase everything
gfloyd_clean <- gfloyd2 %>%
  dplyr::select(clean_text) %>%
  unnest_tokens(word, clean_text)

## Removing unnessary words ##
# Load stop word dictionary #
data("stop_words")

# Count unique words
nrow(gfloyd_clean)

# Remove all stop words
gfloyd_clean <-gfloyd_clean %>%
  anti_join(stop_words)

# Count all the uniques words again
nrow(gfloyd_clean)


### 1st Analysis Top N Words ###
## plot a graph of top 10 words ##
gfloyd_clean %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Frequency of Words",
       x = "Words",
       title = "Top 20 most used words in tweets",
       subtitle = "Stop words have been removed")

## Remove certain words (then run top10 words again)
gfloyd_clean<-gfloyd_clean[ grep("george", gfloyd_clean$word, invert = TRUE),]
gfloyd_clean<-gfloyd_clean[ grep("floyd", gfloyd_clean$word, invert = TRUE),]
gfloyd_clean<-gfloyd_clean[ grep("blacklivematter", gfloyd_clean$word, invert = TRUE),]
gfloyd_clean<-gfloyd_clean[ grep("chauvin", gfloyd_clean$word, invert = TRUE),]
gfloyd_clean<-gfloyd_clean[ grep("derek", gfloyd_clean$word, invert = TRUE),]
gfloyd_clean<-gfloyd_clean[ grep("blacklivesmatter", gfloyd_clean$word, invert = TRUE),]
gfloyd_clean<-gfloyd_clean[ grep("blacklivesmatters", gfloyd_clean$word, invert = TRUE),]


### 2nd Analysis : Sentiment analysis result (positive/negative) ###
## Sentiment by bing(pos/neg) ##
sentiments_tweet <- gfloyd_clean %>%  
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("red2", "blue3")) +
  facet_wrap(~sentiment, scales = "free_y") +
  ylim(0, 750) +
  labs(y = NULL, x = NULL) +
  coord_flip() +
  theme_minimal()

# Show sentiment graph #
sentiments_tweet


### 3nd Analysis : Sentiment analysis result (by emotions ###
## Sentiment by NRC (discrete emotions) ##
sentiments_nrc <- gfloyd_clean %>%  
  inner_join(get_sentiments("nrc"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  ylim(0, 750) +
  labs(y = NULL, x = NULL) +
  coord_flip() +
  theme_minimal()

#show NRC graph
sentiments_nrc


### 4th Analysis: Create Basic Wordcloud ###
## Create Word Cloud ##
library(wordcloud)
# Assign multiple colors with color brewer
# 참조: https://m.blog.naver.com/hsj2864/220978821195
pal <-brewer.pal(8, "Dark2")

# 그림이 짤릴 경우 아래 명령어를 쳐서 윈도우를 열고 다시 출력
dev.new(width = 1000, height = 1000, unit = "px")

# 워드클라우드 출력 
gfloyd_clean %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, ordered.colors = FALSE,scale=c(3.5,.5),
                 colors=pal,max.words = 100))


### 5th Analysis: Create wordcloud COMBINED WITH sentiment (Pos/Neg) ###

# 그림이 짤릴 경우 아래 명령어를 쳐서 윈도우를 열고 다시 출력 #
dev.new(width = 1000, height = 1000, unit = "px")

# Create word cloud with sentiment (Pos/Neg)
library(wordcloud)
library(reshape2)
gfloyd_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(title.colors = c("red2","blue3"),
                   title.bg.colors = c("white","white"),
                   colors = c("red2","blue3"),
                   max.words = 100)


### 6th Analysis: Create wordcloud COMBINED WITH sentiment (Pos/Neg) ###

# 카테고리가 많아서 그림이 크므로 창을 더 키울것 
dev.new(width = 2000, height = 2000, unit = "px")

# Create word cloud with sentiment (discrete emotion)
pal2 <-brewer.pal(10, "Paired")
gfloyd_clean %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(title.colors = c("forestgreen","dodgerblue4","darkorchid4",
                                    "darkred","darkorange2","hotpink3",
                                    "lightsalmon3","seashell4","turquoise4",
                                    "slateblue"),
                   title.bg.colors = "grey95",
                   scale = c(5,1),
                   title.size = 1.5,
                   colors = c("forestgreen","dodgerblue4","darkorchid4",
                              "darkred","darkorange2","hotpink3",
                              "lightsalmon3","seashell4","turquoise4",
                              "slateblue"),
                   max.words = 155)
