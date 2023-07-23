
### Step 1: Load Required Packages

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
library(knitr)
library(kableExtra)


### Step 2: Import Datasets


# Set the working directory to the location of the datasets
setwd("C:/Users/mayjaikaew/R-project/raw-data")

# Import the 'Pet Supplies' dataset
pet <- stream_in(file("Pet_Supplies_5.json.gz", "r"))

# Import the 'Grocery and Gourmet Food' dataset
grocery <- stream_in(file("Grocery_and_Gourmet_Food_5.json.gz", "r"))


### Step 3: Prepare the Data


# Convert review texts to lowercase and create tibbles
pet.tibble <- tibble(text = str_to_lower(pet$reviewText))
grocery.tibble <- tibble(text = str_to_lower(grocery$reviewText))


### Step 4: Write Tibbles to CSV


# Specify the path to save the CSV files
path <- "C:/Users/mayjaikaew/R-project/raw-data"

# Write the 'pet' tibble to CSV
write.csv(pet.tibble, file = file.path(path, "pet.tibble.csv"))

# Write the 'grocery' tibble to CSV
write.csv(grocery.tibble, file = file.path(path, "grocery.tibble.csv"))




#get data into tibble 
pet.tibble <- tibble(text = str_to_lower(pet$reviewText))
grocery.tibble <- tibble(text = str_to_lower(grocery$reviewText))

#write csv from tibble
path <- "C:/Users/mayjaikaew/R-project/raw-data"
write.csv(pet.tibble, file = file.path(path, "pet.tibble.csv"))
write.csv(grocery.tibble, file = file.path(path, "grocery.tibble.csv"))


### Cleaning data

# Get stopwords from 'tidytext' package 
data(stop_words)
stopwords <- stop_words

# Cleaning data
#remove  stopwords from data frame and unnest words into token separately

pet.nosw <- pet.tibble %>%  
  unnest_tokens(output = "words", input = "text") %>% 
  anti_join(stop_words, by = c("words" = "word"))

gro.nosw <- grocery.tibble %>%  
  unnest_tokens(output = "words", input = "text") %>% 
  anti_join(stop_words, by = c("words" = "word"))

## Basic Sentiment : word level
# Which words appear most frequently in each category?

#grocery product
#count for word cloud
gro_wc <- gro.nosw %>% 
  group_by(words) %>% 
  count() %>% 
  arrange(-n)

#calculate as percentage
gro_word_count <- gro.nosw %>% 
  group_by(words) %>% 
  count() %>% 
  rename(count = n) %>%  # Renaming the 'n' column to 'count' 
  mutate(percentage = round(count * 100 / gro_total_wordcount, 2)) %>% 
  arrange(-percentage) %>% 
  head(10)

#visualization on wordcloud
wordcloud(
  gro_wc$words,
  gro_wc$n, 
  random.order=FALSE,
  max.words = 100,
  rot.per=0.2,
  scale = c(3, 0.8),
  colors=brewer.pal(8, "Dark2"),
  use.r.layout=TRUE)

#pet supplies
pet_total_wordcount <- nrow(pet.nosw)

#count for word cloud
pet_wc <- pet.nosw %>% 
  group_by(words) %>% 
  count() %>% 
  arrange(-n)

#calculate as percentage
pet_word_count <- pet.nosw %>% 
  group_by(words) %>% 
  count() %>% 
  rename(count = n) %>%  # Renaming the 'n' column to 'count'
  mutate(percentage = round(count *100/ pet_total_wordcount , 2)) %>% 
  arrange(-count) %>% 
  head(10)

#visualization on wordcloud
wordcloud(
  pet_wc$words,
  pet_wc$n, 
  random.order=FALSE,
  max.words = 100,
  rot.per=0.3,
  scale = c(3, 0.8),
  colors=brewer.pal(8, "Dark2"),
  use.r.layout=TRUE)


# Bing, classifies words from each sentence into positive and negative sentiment.

#Apply sentiment to words
pet.bing <- pet.nosw %>% 
  inner_join(get_sentiments("bing"), by = c("words" = "word")) %>% 
  count(words, sentiment, sort = TRUE)

gro.bing <- gro.nosw %>% 
  inner_join(get_sentiments("bing"), by = c("words" = "word")) %>% 
  count(words, sentiment, sort = TRUE)

#get most popular top 10 positive and negative words found.
pet.bing.top <- pet.bing %>%
  group_by(sentiment) %>%
  top_n(10)

gro.bing.top <-gro.bing %>%
  group_by(sentiment) %>%
  top_n(10) 

## Create Kable to demonstrate the data
# Define the table caption and subcaption
caption <- "Bing Lexicon Analysis"
subcaptions <- c("Grocery products", "Pet Supplies")

# Create the first table on the left
table_left <- kable(gro.bing.top, caption = "Grocery products") %>%
  kable_styling(full_width = FALSE)

# Create the second table on the right
table_right <- kable(pet.bing.top, caption = "Pet Supplies") %>%
  kable_styling(full_width = FALSE)

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

====================================================
  
#tidy up for plotting the result
pet.bing.top.vis <- pet.bing %>%
  group_by(sentiment) %>%
  top_n(10) %>% 
  mutate(dataset= 'pet')

gro.bing.top.vis <-gro.bing %>%
  group_by(sentiment) %>%
  top_n(10) %>% 
  mutate(dataset= 'grocery')

bing.combind <- rbind(pet.bing.top.vis,gro.bing.top.vis)

bing.combind %>%
  ungroup() %>%
  mutate(words = reorder(words, n)) %>%
  arrange(dataset, sentiment, desc(n)) %>%
  ggplot(aes(words, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ dataset + sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

===============================================================

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

## Create kable
# Pet supplies kable
kable(pet.nrc.ws.perc,caption = "Pet Supplies")

# Grocery products kable
kable(gro.nrc.ws.perc,caption = "Grocery products")


#visualization in bar chart
nrc.combined <- rbind(gro.nrc.ws.perc,pet.nrc.ws.perc)
ggplot(data= nrc.combined ,aes(x=sentiment,y=n ,label=n)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  facet_wrap(~ dataset, scales = "free_y")+
  labs(title = "NRC Sentiment Analysis Comparison between Grocery Products and Pet Supplies' reviews")+
  theme(plot.title = element_text(size = 10))

#stacked bar chart in percentage
ggplot(data = nrc.combined, aes(x = "", y = percentage, fill = sentiment)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) +  # Adjust the label size here
  theme_minimal() +
  labs(title = "NRC Sentiment Analysis Comparison 
       between Grocery and Pet product review") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 8)) +  # Adjust the axis text size here
  facet_wrap(~ dataset, scales = "free_y")

#It shown that pet supplies has more percentage of negative-group words than grocery product.

#Analysis by Afinn Lexicon

# Apply sentiment to words for 'pet' and 'grocery' datasets
gro.afinn <- gro.nosw %>% 
  inner_join(get_sentiments("afinn"), by = c("words" = "word"))

pet.afinn <- pet.nosw %>% 
  inner_join(get_sentiments("afinn"), by = c("words" = "word"))

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


#Visualization in scatter plot to compare two data
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
  ggtitle("AFINN Sentiment Analysis Pet Supplies") +
  theme_bw()+
  xlim(0,3500)+
  scale_color_manual(values = c("blue", "red"),
                     breaks = c(TRUE, FALSE))+
  theme(legend.position = "none")  # Hide the legend