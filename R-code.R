# load required packages
library(tidyverse)
library(syuzhet)
library(tidytext)
library(jsonlite)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(tidyr)
library(ggplot2)
library(reshape2)


# import  datasets
setwd("C:/Users/mayjaikaew/R-project/raw-data")

#pet supplies 2098325 observations
pet <- stream_in(file("Pet_Supplies_5.json.gz","r"))

#grocery and gourmet food 1143860 observations
grocery <- stream_in(file("Grocery_and_Gourmet_Food_5.json.gz","r"))


#get data into tibble 
lux.tibble <- tibble(text = str_to_lower(lux$reviewText))
pet.tibble <- tibble(text = str_to_lower(pet$reviewText))
grocery.tibble <- tibble(text = str_to_lower(grocery$reviewText))

#write csv from tibble
path <- "C:/Users/mayjaikaew/R-project/raw-data"
write.csv(pet.tibble, file = file.path(path, "pet.tibble.csv"))
write.csv(grocery.tibble, file = file.path(path, "grocery.tibble.csv"))



#Basic Sentiment Analysis; focus on word level

## Get stopwords from 'tidytext' package 
data(stop_words)
stopwords <- stop_words

# Cleaning data
#remove  stopwords from data frame and unnest words into token separately
lux.nosw <- lux.tibble %>%  
  unnest_tokens(output = "words", input = "text") %>% 
  anti_join(stop_words, by = c("words" = "word"))

pet.nosw <- pet.tibble %>%  
  unnest_tokens(output = "words", input = "text") %>% 
  anti_join(stop_words, by = c("words" = "word"))


gro.nosw <- grocery.tibble %>%  
  unnest_tokens(output = "words", input = "text") %>% 
  anti_join(stop_words, by = c("words" = "word"))


# Which words appear most frequently in each category?

#grocery product
gro_word_count <- gro.nosw %>% 
  group_by(words) %>% 
  count() %>% 
  arrange(-n)

wordcloud(
  gro_word_count$words,
  gro_word_count$n, 
  random.order=FALSE,
  max.words = 200,
  rot.per=0.2,
  scale = c(4, 0.1),
  colors=brewer.pal(8, "Dark2"),
  use.r.layout=TRUE)

#pet supplies
pet_word_count <- pet.nosw %>% 
  group_by(words) %>% 
  count() %>% 
  arrange(-n)

wordcloud(
  pet_word_count$words,
  pet_word_count$n, 
  random.order=FALSE,
  max.words = 200,
  rot.per=0.3,
  scale = c(6, 0.1),
  colors=brewer.pal(8, "Dark2"),
  use.r.layout=TRUE)


#Get words from all sentiments
get_sentiments("bing")
get_sentiments("nrc")
get_sentiments("afinn")

# Bing, classifies words from each sentence into positive and negative sentiment.

#Apply sentiment to words
pet.bing <- pet.nosw %>% 
  inner_join(get_sentiments("bing"), by = c("words" = "word")) %>% count(words, sentiment, sort = TRUE)


gro.bing <- gro.nosw %>% 
  inner_join(get_sentiments("bing"), by = c("words" = "word")) %>% count(words, sentiment, sort = TRUE)

#get most popular top 10 positive and negative words found.

pet.bing.top <- pet.bing %>%
  group_by(sentiment) %>%
  top_n(10) %>% 
  mutate(dataset= 'pet')

gro.bing.top <-gro.bing %>%
  group_by(sentiment) %>%
  top_n(10) %>% 
  mutate(dataset= 'grocery')

#tidy up for plotting the result
bing.combind <- rbind(pet.bing.top,gro.bing.top)


#Visualization in Bing with top 10 most sentiment expression

bing.combind %>%
  ungroup() %>%
  mutate(words = reorder(words, n)) %>%
  arrange(dataset, sentiment, desc(n)) %>%
  ggplot(aes(words, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ dataset + sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

gro.bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(words = reorder(words, n)) %>%  # Reorder the 'words' variable based on 'n'
  ggplot(aes(words, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

pet.bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(words = reorder(words, n)) %>%  # Reorder the 'words' variable based on 'n'
  ggplot(aes(words, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()



#NRC lexicon, classifies words into emotions like positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.

#Apply sentiment to words
gro.nrc <- gro.nosw %>% 
    inner_join(get_sentiments("nrc"),by = c("words" = "word"))

pet.nrc <- pet.nosw %>% 
  inner_join(get_sentiments("nrc"),by = c("words" = "word"))
  
#sum number of each sentiment
gro.nrc.ws <- gro.nrc %>% 
  group_by(sentiment) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  mutate(sentiment = reorder(sentiment, n)) 

pet.nrc.ws <- pet.nrc %>% 
  group_by(sentiment) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  mutate(sentiment = reorder(sentiment, n)) 


# Calculate the total number of sentiments
gro.total <- sum(gro.nrc.ws$n)
pet.total <- sum(pet.nrc.ws$n)

# Calculate the percentage of each sentiment
gro.nrc.ws.perc <- gro.nrc.ws %>%
  mutate(percentage = n / gro.total * 100) %>% 
  mutate(dataset = 'grocery')

pet.nrc.ws.perc <- pet.nrc.ws %>%
  mutate(percentage = n / pet.total * 100) %>% 
  mutate(dataset = 'pet')

nrc.combined <- rbind(gro.nrc.ws.perc,pet.nrc.ws.perc)

#Visualization in pie chart

ggplot(data = gro.nrc.ws.perc, aes(x = "", y = percentage, fill = sentiment)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(legend.position = "bottom")+
  labs(title = 'NRC sentiment analysis of grocery and food product')+
  theme(legend.position = "bottom")

ggplot(data = pet.nrc.ws.perc, aes(x = "", y = percentage, fill = sentiment)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(legend.position = "bottom")+
  labs(title = 'NRC sentiment analysis of pet product')+
  theme(legend.position = "bottom")

#visualization in bar chart

ggplot(data= nrc.combined ,aes(x=sentiment,y=n ,label=n)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  facet_wrap(~ dataset, scales = "free_y")+
  labs(title = "NRC Sentiment Analysis Comparison between Grocery and Pet product review")+
  theme(plot.title = element_text(size = 10))

#stacked bar chart in percentage

ggplot(data = nrc.combined, aes(x = "", y = percentage, fill = sentiment)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  labs(title = "NRC Sentiment Analysis Comparison between Grocery and Pet product review") +
  theme(legend.position = "bottom") +
  facet_wrap(~ dataset, scales = "free_y")

#It shown that pet supplies has more percentage of negative-group words than grocery product.

#Analysis by Afinn Lexicon

#Apply sentiment to words
gro.afinn <- gro.nosw %>% 
  inner_join(get_sentiments("afinn"),by = c("words" = "word"))

pet.afinn <- pet.nosw %>% 
  inner_join(get_sentiments("afinn"),by = c("words" = "word"))

#Counting on how many words have found 
gro.afinn.count <- gro.afinn %>% count(words)

gro.afinn.join <- gro.afinn %>%
  inner_join(gro.afinn.count, by = "words")

gro.afinn.cat <- gro.afinn.join %>%
  mutate(sentiment_category = ifelse(value > 0, "positive",
                                     ifelse(value < 0, "negative", "neutral")))
#Afinn on pet product
pet.afinn.count <- pet.afinn %>% count(words)

pet.afinn.join <- pet.afinn %>%
  inner_join(pet.afinn.count, by = "words")

pet.afinn.cat <- pet.afinn.join %>%
  mutate(sentiment_category = ifelse(value > 0, "positive",
                                     ifelse(value < 0, "negative", "neutral")))


#Visualization in box plot to compare two data
ggplot(gro.afinn.join, aes(n , value ,color = value > 0)) +
  geom_point() +
  xlab("Number of Reviews") +
  ylab("Sentiment Score") +
  ggtitle("AFINN Sentiment Analysis Grocery product") +
  theme_bw()+
  xlim(0,3500)+
  scale_color_manual(values = c("blue", "red"),
                     breaks = c(TRUE, FALSE))+
  theme(legend.position = "none")  # Hide the legend
  
ggplot(pet.afinn.join, aes(n , value ,color = value > 0)) +
  geom_point() +
  xlab("Number of Reviews") +
  ylab("Sentiment Score") +
  ggtitle("AFINN Sentiment Analysis Grocery product") +
  theme_bw()+
  xlim(0,3500)+
  scale_color_manual(values = c("blue", "red"),
                     breaks = c(TRUE, FALSE))+
  theme(legend.position = "none")  # Hide the legend


