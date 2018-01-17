library(tidyverse)
library(tidytext)
library(gutenbergr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# download the text file by gutenberg_id
full_text <- gutenberg_download(1342)
tidy_text <- full_text %>% 
  unnest_tokens(word, text)
tidy_text <- tidy_text %>%
  anti_join(stop_words)
tidy_text <- tidy_text %>%
  count(word, sort = T)
tidy_plot <- tidy_text[1:10,]
tidy_plot %>% 
  ggplot(aes(word, n)) + 
  geom_col() + 
  xlab(NULL) +
  coord_flip()
tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, sort = T)

###################################
full_collection<- gutenberg_download(c(1342, 158, 161, 141), meta_fields = "title")
full_collection %>% 
  count(title)
book_words<- full_collection %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(title, word, sort=T)
book_words<- book_words %>%
  bind_tf_idf(word, title, n) %>%
  arrange(-tf_idf)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_hgwells %>%
  count(word, sort = TRUE)

bonte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bonte <- bonte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_bonte %>%
  count(word, sort = TRUE)

# str_extract() extract the words between _ and italic, extracting all format, and treat _word_ and word in same way
frequency<- bind_rows(mutate(tidy_bonte, author = "Bronte Sisters"),
                      mutate(tidy_hgwells, author = "H.G. Wells"),
                      mutate(tidy_text, author = "Jane Austen")) %>%
  mutate(word = str_extract(word, "[a-z,]+")) 

frequency <- frequency %>%
  count(author, word)

frequency <- frequency %>%
  group_by(author) 

frequency2 <- frequency %>%
  mutate(proportion = n / sum(n) ) 

frequency2 <- frequency2 %>%
  select(-n) # select() keeps only the variables you mention, -nn means delete nn from dataframe

frequency2 <- frequency2 %>%
  spread(author, proportion) 
# spread author as the colnames, and fill in value -> proportion

frequency2 <- frequency2 %>%
  gather(author, proportion, `Bronte Sisters`:`H.G. Wells`)
# gather(), gather consolidate the author bronte sister and H.G. Wells into one column and then generate another column called proportion

##################################################################################################




