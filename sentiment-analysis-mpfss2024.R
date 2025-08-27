# Load necessary libraries
library(tidyverse)
library(lubridate)
library(tidytext)
library(textdata)
library(sentimentr)
library(igraph)
library(ggraph)
library(tm)
library(wordcloud)
library(quanteda)
library(ggplot2)
library(scales)
library(ggrepel)
library(kableExtra)
# Read the data and Ensure created_at is in datetime format
mpfss_tweets <- read.csv("data/updated_usethis-MPFSS2024.csv") %>% 
  mutate(created_at = format(mdy_hm(created_at), "%Y-%m-%d %H:%M")) %>% 
  mutate(value_generated = 2 * retweet_count + #assigning weight on patterns
                           1 * favorite_count +
                           1.5 * retweet_retweet_count +
                          1 * retweet_favorite_count)


# Filter for March-April tweets only(Has been posted on 18 march 2024)
tweets_march <- mpfss_tweets %>%
  filter(month(created_at) %in% c(3, 4))  # Months 3 (March) and 4 (April)

# Filter for September–Mctober tweets only (Have  been posted on  25 sept 2024)
tweets_Sept <- mpfss_tweets %>%
  filter(month(created_at) %in% c(9, 10))  # Months 8 (Aug) and 9 (Sept)
#-------------------------------------------------------------------------------

#Hourly tweet Trend
tweets_Sept %>% 
  separate(created_at, into = c("date", "time"), sep = " ") %>% 
  mutate(time = hm(time)) %>% 
  mutate(hour = hour(time)) %>% 
  mutate(date = ymd(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(month = ifelse(month %in% c(9,10) , "September MPFSS2024", month)) %>% 
  group_by(hour, month) %>% 
  count() %>% 
  ggplot(aes(factor(hour), n, fill = as.character(month))) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = "#AB892C") +
  facet_wrap(~month, ncol = 2, scales = "free") +
  labs(x = "Hours of the Day", y = "Tweet Counts", title = "Tweets Count by Hour of Day") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )  
# Twitter Engagement Analysis
#----------------------------

# Engagement metrics
engagement_summary <- function(df, label) {
  df %>%
    summarise(
      period = label,
      likes = sum(favorite_count, na.rm = TRUE),
      retweets = sum(retweet_count, na.rm = TRUE),
      replies = 0, #set to 0 if not available
      value_generated = sum(value_generated, na.rm = TRUE),
      Eng_momentum = sum(retweet_retweet_count, na.rm = TRUE),
      Eng_depth = sum(retweet_favorite_count,na.rm = TRUE),
      Eng_Ratio = (likes + retweets) / sum(followers_count, na.rm = TRUE),
      First_order_Eng_Ratio = retweets/(retweets+likes),
      Second_order_Eng_Ratio = Eng_momentum/Eng_depth,
      ROE = value_generated / (likes + retweets + replies),
      total_tweets = n()
      
    )
}

summary_mpfss <- engagement_summary(mpfss_tweets %>% mutate(retweet_count=round(retweet_count,0)), "MPFSS2024")
summary_feb_mar <- engagement_summary(tweets_march %>% mutate(retweet_count=round(retweet_count,0)), "March-2024")
summary_aug_sept <- engagement_summary(tweets_Sept %>% mutate(retweet_count=round(retweet_count,0)), "Sept-2024")

# Combine summaries
engagement_data <- bind_rows(summary_feb_mar, summary_aug_sept)

# Plot comparison
ggplot(engagement_data, aes(x = period)) +
  geom_col(aes(y = likes, fill = "Likes"), position = "stack") +
  geom_col(aes(y = retweets, fill = "Retweets"), position = "stack") +
  geom_col(aes(y = Eng_momentum, fill = "Engagement Momentum"), position = "stack") +
  geom_col(aes(y = Eng_depth, fill = "Engagement Depth"), position = "stack") +
  labs(title = "Twitter Engagement Comparison: MPFSS2024",
       y = "Average per Tweet", x = "Period") +
  scale_fill_manual(values = c("Likes" = "#753918", "Retweets" = "#AB892C", 
                               "Engagement Momentum" = "#FCF7EA", "Engagement Depth" = "#89724E")) +
  theme_bw()
#----------------------
# Reshape your data to long format
engagement_long <- engagement_data %>%
  pivot_longer(cols = c(likes, retweets, Eng_momentum, Eng_depth),
               names_to = "Metric",
               values_to = "Value")


ggplot(engagement_long, aes(x = period, y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  labs(title = "Twitter Engagement Comparison: MPFSS2024",
       y = "Average per Tweet", x = "Period") +
  scale_fill_manual(values = c(
    "likes" = "#753918", 
    "retweets" = "#AB892C", 
    "Eng_momentum" = "#FCF7EA", 
    "Eng_depth" = "#89724E"
  )) +
  theme_bw()

#-------------------
# Clean text

clean_text <- function(text) {
  text %>%
    str_to_lower() %>%
    str_replace_all("https?://[[:alnum:]./]+", "") %>% # Remove URLs
    str_replace_all("@[[:alnum:]_]+", "") %>% # Remove mentions
    str_replace_all("[[:punct:]]", "") %>% # Remove punctuation
    str_replace_all("[[:digit:]]", "") %>% # Remove numbers
    str_replace_all("\\s+", " ") %>% # Remove extra whitespace
    str_trim() # Trim leading/trailing whitespace
}

mpfss_tweets <- mpfss_tweets %>%
  mutate(clean_text = clean_text(text))

tweets_march <- tweets_march %>% 
  mutate(clean_text = clean_text(text))

tweets_Sept <- tweets_Sept %>% 
  mutate(clean_text = clean_text(text))


## 2. Sentiment Analysis ----

# Get sentiment scores using sentimentr (more nuanced than AFINN)
sentiment_scores <- sentiment_by(mpfss_tweets$clean_text)
mpfss_tweets <- mpfss_tweets %>%
  mutate(sentiment = sentiment_scores$ave_sentiment)

#March sentiment
sentiment_scores_march <- sentiment_by(tweets_march$clean_text)
mpfss_march_tweets <- tweets_march %>%
  mutate(sentiment = sentiment_scores_march$ave_sentiment)

#September Sentiment

sentiment_scores_sep <- sentiment_by(tweets_Sept$clean_text)

mpfss_sept_tweets <- tweets_Sept %>%
  mutate(sentiment = sentiment_scores_sep$ave_sentiment)

# Categorize sentiments
mpfss_tweets <- mpfss_tweets %>%
  mutate(sentiment_category = case_when(
    sentiment > 0.1 ~ "Positive",
    sentiment < -0.1 ~ "Negative",
    TRUE ~ "Neutral"
  ))

#March
mpfss_tweets_march <- mpfss_march_tweets %>%
  mutate(sentiment_category = case_when(
    sentiment > 0.1 ~ "Positive",
    sentiment < -0.1 ~ "Negative",
    TRUE ~ "Neutral"
  ))
#September
mpfss_tweets_sept <- mpfss_sept_tweets %>%
  mutate(sentiment_category = case_when(
    sentiment > 0.1 ~ "Positive",
    sentiment < -0.1 ~ "Negative",
    TRUE ~ "Neutral"
  ))


# Sentiment summary
sentiment_summary <- mpfss_tweets %>%
  count(sentiment_category) %>%
  mutate(percentage = n / sum(n) * 100)

#March

sentiment_summary_march <- mpfss_tweets_march %>%
  count(sentiment_category) %>%
  mutate(percentage = n / sum(n) * 100)

#September

sentiment_summary_sept <- mpfss_tweets_sept %>%
  count(sentiment_category) %>%
  mutate(percentage = n / sum(n) * 100)

#Emotions
library(syuzhet)

#MPFSS2024
emotions <- get_nrc_sentiment(mpfss_tweets$clean_text)

emotion_summary <- colSums(emotions)
emotion_df <- data.frame(Emotion = names(emotion_summary), Count = emotion_summary)

emotion_df <- emotion_df %>% 
  arrange(desc(Count)) %>% 
  mutate(
    percent = round(Count / sum(Count) * 100, 1),
    label = paste0(Emotion, ": ", percent, "%")
  )
#March 2024

emotions_march <- get_nrc_sentiment(tweets_march$clean_text)

emotion_summary_march <- colSums(emotions_march)
emotion_march_df <- data.frame(Emotion = names(emotion_summary_march), Count = emotion_summary_march)

emotion_march_df <- emotion_march_df %>% 
  arrange(desc(Count)) %>% 
  mutate(
    percent = round(Count / sum(Count) * 100, 1),
    label = paste0(Emotion, ": ", percent, "%")
  )

#Sept 2024

emotions_sept <- get_nrc_sentiment(tweets_Sept$clean_text)

emotion_summary_sept <- colSums(emotions_sept)
emotion_sept_df <- data.frame(Emotion = names(emotion_summary_sept), Count = emotion_summary_sept)

emotion_sept_df <- emotion_sept_df %>% 
  arrange(desc(Count)) %>% 
  mutate(
    percent = round(Count / sum(Count) * 100, 1),
    label = paste0(Emotion, ": ", percent, "%")
  )

# Plot sentiment distribution

ggplot(sentiment_summary, aes(x = sentiment_category, y = n, fill = sentiment_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(title = "Sentiment Distribution of #MPFSS2024 Tweets",
       x = "Sentiment Category",
       y = "Count") +
  theme_minimal()

#Pie Charts 

ggplot(sentiment_summary_sept, aes(x = "", y = percentage, fill = sentiment_category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(
    aes(label = paste0(round(percentage, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "#231F20",
    size = 4
  ) +
  scale_fill_manual(values = c(
    "Positive" = "#AB892C",
    "Negative" = "#753918",
    "Neutral"  = "#FCF7EA"
  )) +
  labs(title = "Sentiment Distribution of Sept. MPFSS2024 Tweets",
       fill = "Sentiments:") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "bottom"  # you can set to "right" to hide or "none" if you prefer
  )

#Charts Emotions

ggplot(emotion_march_df, aes(x = reorder(Emotion, -percent), y = percent)) +  # Remove `fill = Emotion` from aes()
  geom_col(fill = "#AB892C") +  # Set static color for all bars
  theme_bw() +
  geom_text(aes(label = paste0(percent, "%")), vjust = -0.5) +
  labs(
    title = "Sept MPFSS2024 Reactions Distribution", 
    x = "Reaction Categories", 
    y = "Percentage Rates"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none",  # No legend needed (single color),
    # Background modifications:
    plot.background = element_rect(fill = "#FCF7EA"),  # Graph area background
    panel.background = element_rect(fill = "#FCF7EA"), # Panel background
    panel.grid.major = element_line(color = "white"),  # Optional: light grid lines
    panel.grid.minor = element_line(color = "white")   # Optional: light grid lines
  )

# Word cloud by sentiment
#March

positive_words_march <- mpfss_tweets_march %>%
  filter(sentiment_category == "Positive") %>%
  unnest_tokens(word, clean_text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
#September

positive_words_sept <- mpfss_tweets_sept %>%
  filter(sentiment_category == "Positive") %>%
  unnest_tokens(word, clean_text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

wordcloud2(positive_words_sept, shape = "circle", color = "#AB892C")  

negative_words <- mpfss_tweets_sept %>%
  filter(sentiment_category == "Negative") %>%
  unnest_tokens(word, clean_text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

library(wordcloud2)

par(mfrow = c(2, 1))
# Create round word clouds
wordcloud2(positive_words, size = 0.8, shape = 'circle', color = "darkgreen")
wordcloud2(negative_words, size = 0.8, shape = 'circle', color = "darkred")


## 3. Network Analysis ----
# Create edge list from retweets and mentions

retweet_edges <- mpfss_tweets %>%
  filter(is_retweet == TRUE) %>%
  select(screen_name, retweet_screen_name) %>%
  rename(from = retweet_screen_name, to = screen_name) %>%
  filter(!is.na(from) & !is.na(to))

mention_edges <- mpfss_tweets %>%
  mutate(mentions_screen_name = str_extract_all(text, "@[[:alnum:]_]+")) %>%
  unnest(mentions_screen_name) %>%
  mutate(mentions_screen_name = str_remove(mentions_screen_name, "@")) %>%
  select(retweet_screen_name, mentions_screen_name) %>%
  rename(from = retweet_screen_name, to = mentions_screen_name) %>%
  filter(to %in% mpfss_tweets$retweet_screen_name) # Only include mentions of users in our dataset

# Combine edges
all_edges <- retweet_edges %>%
  group_by(from, to) %>%
  summarise(weight = n()) %>%
  ungroup()

# Create graph
twitter_graph <- graph_from_data_frame(all_edges, directed = TRUE)

# Calculate network metrics
V(twitter_graph)$degree <- degree(twitter_graph, mode = "all")
V(twitter_graph)$betweenness <- betweenness(twitter_graph)
V(twitter_graph)$closeness <- closeness(twitter_graph)
V(twitter_graph)$eigen_centrality <- eigen_centrality(twitter_graph)$vector
#-----------------------------------------------------------------------------
# Extract vertex info
vertex_info <- as_data_frame(twitter_graph, what = "vertices")

# Identify key influencers
top_influencers <- vertex_info %>%
  arrange(desc(degree)) %>%
  head(10)

# Plot network
set.seed(123)
ggraph(twitter_graph, layout = "fr") + 
  geom_edge_link(aes(alpha = weight), show.legend = FALSE) + 
  geom_node_point(aes(size = degree, color = betweenness)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_gradient(low = "lightblue", high = "red") +
  labs(title = "Twitter Network for #MPFSS2024",
       subtitle = "Node size by degree, color by betweenness centrality") +
  theme_void()

# Community detection
communities <- cluster_louvain(as.undirected(twitter_graph))
V(twitter_graph)$community <- communities$membership

# Plot with communities
ggraph(twitter_graph, layout = "fr") + 
  geom_edge_link(aes(alpha = weight), show.legend = FALSE) + 
  geom_node_point(aes(size = degree, color = as.factor(community))) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  labs(title = "Twitter Communities for #MPFSS2024",
       subtitle = "Colors represent different communities") +
  theme_void()

## 4. Create Final Data Matrix ----

# Combine tweet data with network metrics
user_data <- mpfss_tweets %>%
  group_by(retweet_screen_name) %>%
  summarise(
    tweet_count = n(),
    avg_favorite = mean(favorite_count, na.rm = TRUE),
    avg_retweet = mean(retweet_count, na.rm = TRUE),
    followers = first(followers_count),
    following = first(friends_count)
  ) %>%
  left_join(vertex_info, by = c("retweet_screen_name" = "name")) %>%
  mutate(
    influence_score = (degree / max(degree, na.rm = TRUE) * 0.4 + 
                         (betweenness / max(betweenness, na.rm = TRUE)) * 0.3 +
                         (followers / max(followers, na.rm = TRUE)) * 0.3) * 100
  )

# Add sentiment info
user_sentiment <- mpfss_tweets %>%
  group_by(retweet_screen_name) %>%
  summarise(
    avg_sentiment = mean(sentiment, na.rm = TRUE),
    pos_tweets = sum(sentiment_category == "Positive"),
    neg_tweets = sum(sentiment_category == "Negative")
  )

final_matrix <- user_data %>%
  left_join(user_sentiment, by = "retweet_screen_name") %>%
  select(
    retweet_screen_name,
    tweet_count,
    followers,
    following,
    degree,
    betweenness,
    closeness,
    eigen_centrality,
   # community,
    avg_sentiment,
    pos_tweets,
    neg_tweets,
    influence_score
  ) %>%
  arrange(desc(influence_score))

# Display top influencers
kable(final_matrix %>% head(10), caption = "Top 10 Influencers in #MPFSS2024 Network") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

## 5. Preliminary Analysis ----
# Temporal trends
temporal <- mpfss_tweets %>%
  mutate(hour = hour(created_at)) %>%
  count(hour)

ggplot(temporal, aes(x = hour, y = n)) +
  geom_line() +
  geom_point() +
  labs(title = "Temporal Distribution of #MPFSS2024 Tweets",
       x = "Hour of Day",
       y = "Number of Tweets") +
  theme_minimal()

# User engagement
engagement <- mpfss_tweets %>%
  summarise(
    avg_likes = mean(favorite_count),
    avg_retweets = mean(retweet_count),
    avg_replies = mean(reply_count, na.rm = TRUE)
  )

kable(engagement, caption = "Average Engagement Metrics for #MPFSS2024 Tweets") %>%
  kable_styling()

# Most influential tweets
top_tweets <- mpfss_tweets %>%
  arrange(desc(favorite_count + retweet_count)) %>%
  select(screen_name, text, favorite_count, retweet_count, sentiment) %>%
  head(10)

kable(top_tweets, caption = "Top 10 Most Engaging Tweets") %>%
  kable_styling()

# Correlation between sentiment and engagement
correlation <- mpfss_tweets %>%
  summarise(
    cor_sentiment_likes = cor(sentiment, favorite_count),
    cor_sentiment_retweets = cor(sentiment, retweet_count)
  )

kable(correlation, caption = "Correlation Between Sentiment and Engagement") %>%
  kable_styling()