# project form https://data-flair.training/blogs/data-science-r-sentiment-analysis-project/
library(tidytext)#contains sentiment lexicons and dataset called Sentiments
library(tidyverse)
sentiments # dataset with sentiments given to words
get_sentiments("bing") #lexicon model that gives a word a positive or negative meaning.AFINN otoh gives a score from -5 to 5

library(janeaustenr) #provides textual data in the form of books

# arrange data so that each row contains a word
tidy_data <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

# filter the words that correspond to joy from the book Sense and Sensibility
positive_senti <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

tidy_data %>%
  filter(book == "Emma") %>%
  semi_join(positive_senti) %>%
  count(word, sort = TRUE)

#split the data into positive and negative columns and calculate the total sentiment (diff pos/neg)
bing <- get_sentiments("bing")

Emma_sentiment <- tidy_data %>%
  inner_join(bing) %>%
  count(book = "Emma" , index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#visualize the words present in the book Emma
ggplot(Emma_sentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

#count most positive and negative words
counting_words <- tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)
head(counting_words)

#visualize the sentiment score
counting_words %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  coord_flip() +
  labs(y = "Sentiment Score")

#visualize in a wordcloud the most recurring positive and negative words
library(reshape2)
library(wordcloud)
tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"),
                   max.words = 100)