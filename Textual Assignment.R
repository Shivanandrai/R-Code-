#setwd("~/Desktop/Consumer Analytics/New Sessions/Textual Analysis")

#library(rtweet)

rt <- search_tweets(
  "#facebook lang:en", n = 18000,  include_rts = TRUE, parse = TRUE, type = "mixed"
)


happy_df<- rt

#write.csv(happy_df,"~/Desktop/Consumer Analytics/New Sessions/Textual Analysis/rt_database", row.names = FALSE)

getwd()

#install.packages(c("textdata", "tidytext", "widyr", "wordlcloud"))

require("dplyr")
require("tidytext")
require("textdata")
require("widyr")
require("wordcloud")

happy_df <- happy_df[,c(1,5)]
happy_df$user_id<- as.character(happy_df$user_id)
happy_df$text<- as.character(happy_df$text)

sapply(happy_df, function(x) sum(is.na(x)))

skimr::skim(happy_df)

happy_df.tidy <- tidytext::unnest_tokens(happy_df, word, text)
dplyr::count(happy_df.tidy, word, sort = TRUE) 

happy_df.clean <- dplyr::anti_join(happy_df.tidy, tidytext::get_stopwords())

dplyr::count(happy_df.clean, word, sort = TRUE) 

happy_df.count <- dplyr::count(happy_df.clean, word, sort = TRUE) 
happy_df.count <- happy_df.count[which(  happy_df.count$word != "facebook" & 
                                           happy_df.count$word != "https" &
                                           happy_df.count$word != "t.co" &
                                           happy_df.count$word != "may" & 
                                           happy_df.count$word != "social"),]
happy_df.count$word <- reorder(happy_df.count$word, happy_df.count$n)
happy_df.count <- head(happy_df.count, 20)

ggplot2::ggplot(happy_df.count, ggplot2::aes(x = word, y = n)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggpubr::theme_pubclean()


happy_df.count <- dplyr::count(happy_df.clean, user_id, word, sort = TRUE) 
happy_df.count <- happy_df.count[which(happy_df.count$n > 10 & 
                                         happy_df.count$word != "facebook" & 
                                         happy_df.count$word != "https" &
                                         happy_df.count$word != "t.co" &
                                         nchar(happy_df.count$word) > 3),]
head(happy_df.count)

tidytext::bind_tf_idf(happy_df.count, word, user_id, n)

wordcloud::wordcloud(happy_df.count$word, happy_df.count$n, min.freq = 3, max.words = 100, random.order=FALSE)

happy_df.sen <- dplyr::inner_join(happy_df.clean, tidytext::get_sentiments("nrc"), by = "word")
happy_df.sen <- dplyr::inner_join(happy_df.sen, tidytext::get_sentiments("afinn"), by = "word")
head(happy_df.sen, 10)


happy_df.sen_count <- dplyr::count(happy_df.sen, sentiment, word, sort = TRUE)
happy_df.sen_count$word <- reorder(happy_df.sen_count$word, happy_df.sen_count$n)
happy_df.sen_count <- by(happy_df.sen_count, happy_df.sen_count["sentiment"], head, n=5)
happy_df.sen_count <- Reduce(rbind, happy_df.sen_count)

ggplot2::ggplot(happy_df.sen_count, ggplot2::aes(x = word, y = n, fill = sentiment)) +
  ggplot2::geom_col(show.legend = FALSE) +
  ggplot2::facet_wrap(~sentiment, scales = "free") +
  ggplot2::labs(y = "Contribution to sentiment", x = NULL) +
  ggplot2::coord_flip() +
  ggpubr::theme_pubclean()

happy_df.tidy <- tidytext::unnest_tokens(happy_df, bigram, text, token = "ngrams", n = 2)
happy_df.count <- dplyr::count(happy_df.tidy, bigram, sort = TRUE) 
happy_df.count$bigram <- reorder(happy_df.count$bigram, happy_df.count$n)
happy_df.count <- head(happy_df.count, 20)

ggplot2::ggplot(happy_df.count, ggplot2::aes(x = bigram, y = n)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggpubr::theme_pubclean()



#1.Which words are most commonly used in the dataset?
# after removing facebook, https and may, words connected with the censoring of the Russian flag over the Reichstag were the most common words.
#2. Do you find anything interesting when looking at a word cloud of the data?
#once tf-idf is done, the words change to more business and political issues
#3. Does this change if we look at the tf_idf logic? (note: focus on words which have at least been used 5 times)
#Yes
#4. What are the most common sentiments in the tweets?
#mostly negative, anger and fear
#5. Are the posts about facebook rather positive, or rather negative? (note: feel free to apply a simple mean)
#negative

library(rmarkdown)
render(input= "Textual Assignment.R", output_format = "word_document")

knitr::stitch('Textual Assignment.R')
