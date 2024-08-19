### LIBRARY INSTALL + LOAD
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("tidytext")
#install.packages("tidyr")
#install.packages("textdata")
#install.packages("widyr")
#install.packages("ggplot2")

## LOADING OF PACKAGES
library(stringr)
library(dplyr)
library(tidytext)
library(tidyr)
library(textdata)
library(widyr)
library(ggplot2)

## SENTIMENT PACKAGE RETRIEVAL
get_sentiments("nrc")
get_sentiments("bing")
get_sentiments("afinn")

# OUTPUT CHECK -> CHANGE THE DATA FILE TO CHANGE THE NEWS PROVIDER
head(BBCnewsData)
tail(BBCnewsData)

## TOKENIZATION -> CHANGE THE DATA FILE TO CHANGE THE NEWS PROVIDER
news_df <- BBCnewsData %>% select(headline_text)
news_tokens <- news_df %>% unnest_tokens(word, headline_text)

# OUTPUT CHECK
head(news_tokens, 10)
tail(news_tokens, 10)

### WORD FREQUENCY ANALYSIS
news_tokens_count <- news_tokens %>% count(word, sort = TRUE) %>% mutate(proportion = n / sum(n))

# OUTPUT CHECK
head(news_tokens_count, 10)
tail(news_tokens_count, 10)

data(stop_words)
head(stop_words, 10)

### STOP-WORD REMOVAL
news_tokens_no_sp <- news_tokens %>% anti_join(stop_words, by = "word")
head(news_tokens_no_sp, 10)

news_tokens_count <- news_tokens_no_sp %>% count(word, sort = TRUE) %>% mutate(proportion = n/sum(n))
head(news_tokens_count)

news_token_over8000 <- news_tokens_count %>% filter(n > 8000) %>% mutate(word = reorder(word, n))
nrow(news_token_over8000)

news_tokens_count %>% 
  ggplot(aes(word, proportion * 100, fill = ceiling(proportion * 100))) + 
  geom_col() + 
  xlab(NULL) + 
  coord_flip() + 
  theme(legend.position = "none")

# OUTPUT CHECK
head(news_df, 10)

news_df_subset <- news_df[1:1000, , drop = FALSE]

# TOKENIZATION FOR EACH HEADLINE
tkn_1 <- lapply(news_df_subset$headline_text, function(x) {
  data.frame(headline_text = x, stringsAsFactors = FALSE) %>%
    unnest_tokens(word, headline_text)
})

# REMOVE STOP WORDS FROM EACH TOKENIZED DATAFRAME
single_news_tokens <- lapply(tkn_1, function(x) {
  tryCatch({
    if ("word" %in% names(x)) {
      anti_join(x, stop_words, by = "word")
    } else {
      NULL
    }
  }, error = function(e) {
    message("Error processing element: ", e)
    return(NULL)
  })
})

str(single_news_tokens, list.len = 5)
single_news_tokens[[1]]

### SENTIMENT ANALYSIS USING BING'S LEXICON
compute_sentiment <- function(d) {
  if (nrow(d) == 0) {
    return(NA)
  }
  neg_score <- d %>% filter(sentiment == "negative") %>% nrow()
  pos_score <- d %>% filter(sentiment == "positive") %>% nrow()
  pos_score - neg_score
}

sentiments_bing <- get_sentiments("bing")
str(sentiments_bing)

single_news_sentiment_bing <- sapply(single_news_tokens, function(x) {
  if (!is.null(x)) {
    x %>% inner_join(sentiments_bing, by = "word") %>% compute_sentiment()
  } else {
    NA
  }
})

str(single_news_sentiment_bing)

# RESULTS
summary(single_news_sentiment_bing)

single_news_sentiment_bing_df <- data.frame(headline_text = news_df_subset$headline_text, score = single_news_sentiment_bing)

### SENTIMENT ANALYSIS USING NRC'S LEXICON
sentiments_nrc <- get_sentiments("nrc")
unique_sentiments_nrc <- unique(sentiments_nrc$sentiment)

compute_pos_neg_sentiments_nrc <- function(the_sentiments_nrc) {
  s <- unique(the_sentiments_nrc$sentiment)
  df_sentiments <- data.frame(sentiment = s, 
                              mapped_sentiment = c("positive", "negative", "negative", "negative",
                                                   "negative", "positive", "positive", "negative", 
                                                   "positive", "positive"))
  ss <- sentiments_nrc %>% inner_join(df_sentiments, by = "sentiment")
  the_sentiments_nrc$sentiment <- ss$mapped_sentiment
  the_sentiments_nrc
}

nrc_sentiments_pos_neg_scale <- compute_pos_neg_sentiments_nrc(sentiments_nrc)

single_news_sentiment_nrc <- sapply(single_news_tokens, function(x) {
  if (!is.null(x)) {
    x %>% inner_join(nrc_sentiments_pos_neg_scale, by = "word") %>% compute_sentiment()
  } else {
    NA
  }
})

str(single_news_sentiment_nrc)

# RESULTS
summary(single_news_sentiment_nrc)

### SENTIMENT ANALYSIS USING AFINN'S LEXICON
sentiments_afinn <- get_sentiments("afinn")
colnames(sentiments_afinn) <- c("word", "sentiment")
str(sentiments_afinn)

single_news_sentiment_afinn_df <- lapply(single_news_tokens, function(x) {
  if (!is.null(x)) {
    x %>% inner_join(sentiments_afinn, by = "word")
  } else {
    NULL
  }
})

single_news_sentiment_afinn <- sapply(single_news_sentiment_afinn_df, function(x) {
  if (!is.null(x) && nrow(x) > 0) {
    sum(x$sentiment)
  } else {
    NA
  }
})

str(single_news_sentiment_afinn)

# RESULTS
summary(single_news_sentiment_afinn)

single_news_sentiment_afinn_df <- data.frame(headline_text = news_df_subset$headline_text, score = single_news_sentiment_afinn)
head(single_news_sentiment_afinn_df, 10)

### COMPARISON OF THE RESULTS
compute_congruence <- function(x, y, z) {
  v <- c(sign(x), sign(y), sign(z))
  if (sum(is.na(v)) >= 2) {
    return(NA)
  }
  v <- na.omit(v)
  v_sum <- sum(v)
  abs(v_sum) == length(v)
}

compute_final_sentiment <- function(x, y, z) {
  if (is.na(x) && is.na(y) && is.na(z)) {
    return(NA)
  }
  
  s <- sum(x, y, z, na.rm = TRUE)
  ifelse(s > 0, "positive", ifelse(s < 0, "negative", "neutral"))
}

news_sentiments_results <- data.frame(headline_text = news_df_subset$headline_text, 
                                      bing_score = single_news_sentiment_bing, 
                                      nrc_score = single_news_sentiment_nrc, 
                                      afinn_score = single_news_sentiment_afinn,
                                      stringsAsFactors = FALSE)

news_sentiments_results <- news_sentiments_results %>% rowwise() %>% 
  mutate(final_sentiment = compute_final_sentiment(bing_score, nrc_score, afinn_score),
         congruence = compute_congruence(bing_score, nrc_score, afinn_score))

head(news_sentiments_results, 40)

replace_score_with_sentiment <- function(v_score) {
  v_score[v_score > 0] <- "positive"
  v_score[v_score < 0] <- "negative"
  v_score[v_score == 0] <- "neutral"
  v_score
}

news_sentiments_results$bing_score <- replace_score_with_sentiment(news_sentiments_results$bing_score)
news_sentiments_results$nrc_score <- replace_score_with_sentiment(news_sentiments_results$nrc_score)
news_sentiments_results$afinn_score <- replace_score_with_sentiment(news_sentiments_results$afinn_score)

news_sentiments_results[, 2:5] <- lapply(news_sentiments_results[, 2:5], as.factor)

head(news_sentiments_results, 40)

table(news_sentiments_results$bing_score, news_sentiments_results$final_sentiment, dnn = c("bing", "final"))
table(news_sentiments_results$nrc_score, news_sentiments_results$final_sentiment, dnn = c("nrc", "final"))
table(news_sentiments_results$afinn_score, news_sentiments_results$final_sentiment, dnn = c("afinn", "final"))



#-------------------------------------------------------------------------------
# Ensure the data has a column 'headline_text' -> CHANGE THE DATA FILE TO CHANGE THE NEWS PROVIDER
news_df <- BBCnewsData %>% select(headline_text)

# Tokenize the text data
news_tokens <- news_df %>%
  unnest_tokens(word, headline_text)

# Remove stop words
data(stop_words)
news_tokens_no_sp <- news_tokens %>%
  anti_join(stop_words, by = "word")

# Word frequency analysis
news_tokens_count <- news_tokens_no_sp %>%
  count(word, sort = TRUE) %>%
  mutate(proportion = n / sum(n),
         log_n = log(n + 1))  # Add 1 to avoid log(0)

# Define the minimum frequency threshold
min_frequency <- 25  # Adjust as needed 

# Filter words based on the minimum frequency
filtered_words <- news_tokens_count %>%
  filter(n >= min_frequency) %>%
  arrange(desc(n)) %>%
  mutate(rank = row_number(),  # Create a rank column
         word = factor(word, levels = rev(word)))  # Ensure y-axis order

# Create the word map with rank on the y-axis
ggplot(filtered_words, aes(x = log_n, y = rank, size = n, label = word)) +
  geom_text(aes(color = n), show.legend = FALSE) +
  scale_size(range = c(3, 15)) +
  scale_y_reverse() +  # Reverse the y-axis to show higher ranks at the top
  labs(title = "BBC Word Map with Frequency (Log Scale) and Order of Evocation",
       x = "Log Frequency",
       y = "Rank") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10))

# -----------------------------------------------------------------

