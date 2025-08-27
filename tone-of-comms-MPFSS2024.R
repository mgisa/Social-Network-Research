# Tone of communications from MPFSS2024
#--------------------------------------

#PART 1: MPFSS2024 March 2024
library(sentimentr)
library(syuzhet)
library(tidyverse)
library(ggplot2)

text_data <- readLines("data/cleaned_text_mpfss2024-March.txt")
text_data <- paste(text_data, collapse = " ")

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

text_data <- get_sentences(clean_text(text_data))

#March sentiment
march_sentiment_scores <- sentiment_by(text_data)
summary(march_sentiment_scores)

# Categorize sentiments
march_txt_sentiments <- march_sentiment_scores %>%
  mutate(sentiment_category = case_when(
    ave_sentiment > 0.1 ~ "Positive",
    ave_sentiment < -0.1 ~ "Negative",
    TRUE ~ "Neutral"
  ))

# Sentiment summary
sentiment_summary_M <- march_txt_sentiments %>%
  count(sentiment_category) %>%
  mutate(percentage = n / sum(n) * 100)
#Pie Charts 

ggplot(sentiment_summary_M, aes(x = "", y = percentage, fill = sentiment_category)) +
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
  labs(title = "MPFSS March Release Sentiment Distribution",
       fill = "Sentiments:") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "bottom"  # you can set to "right" to hide or "none" if you prefer
  )
# Thematic Tone for March Realses

March_tone <- get_nrc_sentiment(text_data)
tone_summary <- colSums(March_tone)
tone_df <- data.frame(Emotion = names(tone_summary), Count = tone_summary)

march_tone_df <- tone_df %>% 
  arrange(desc(Count)) %>% 
  mutate(
    percent = round(Count / sum(Count) * 100, 1),
    label = paste0(Emotion, ": ", percent, "%")
  )

# Emotion Bar Char
df <- read.csv("data/tone_data_march.csv")

ggplot(march_tone_df, aes(x = reorder(Emotion, -percent), y = percent)) +  # Remove `fill = Emotion` from aes()
         geom_col(fill = "#AB892C") +  # Set static color for all bars
         theme_bw() +
         geom_text(aes(label = paste0(percent, "%")), vjust = -0.5) +
         labs(
           title = "MPFSS September Realease Tone Distribution", 
           x = "Tone Categories", 
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
#-----------------------------------
#March wordcloud
text_march <- data.frame(line = 1, text = text_data) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%   # remove common stop words
  count(word, sort = TRUE)

#September wordcloud

text_sept <- data.frame(line = 1, text = text_sept) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%  # remove common stop words
  count(word, sort = TRUE)


wordcloud2(text_march, shape = 'circle', color = "#AB892C")  

# Force very tight circular constraint
wordcloud2(text_sept, 
           shape = 'circle',
           color = "#AB892C",
           size = 0.5,          # Smaller words
           gridSize = 2,        # Very tight grid
           ellipticity = 1,     # Perfect circle
           minRotation = 0,
           maxRotation = 0,
           shuffle = TRUE)      # Better word distribution









#PART 2: MPFSS2024 September 2024
library(pdftools)

text_sept <- pdf_text("data/MPFSS_March 2024_Final_Booklet.pdf")
# Collapse into a single text
text_sept <- paste(text_sept, collapse = " ")
#text_data_Spt <- readLines("data/cleaned_text_mpfss2024-Sept.txt")
#text_data_Spt <- paste(text_data_Spt, collapse = " ")

# Convert to lowercase, remove numbers, punctuation, and stopwords
text_sept <- data.frame(line = 1, text = text_sept) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)  # remove common stop words

text_data_Spt <- get_sentences(clean_text(text_sept$word))

spt_tone_scores <- sentiment_by(text_data_Spt)
summary(spt_tone_scores)

# Categorize sentiments
sep_txt_sentiments <- spt_tone_scores %>%
  mutate(sentiment_category = case_when(
    ave_sentiment > 0 ~ "Positive",
    ave_sentiment < -0 ~ "Negative",
    TRUE ~ "Neutral"
  ))

# Sentiment summary
sentiment_summary_S <- sep_txt_sentiments %>%
  count(sentiment_category) %>%
  mutate(percentage = n / sum(n) * 100)
#Pie Charts 

ggplot(sentiment_summary_M, aes(x = "", y = percentage, fill = sentiment_category)) +
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
  labs(title = "MPFSS March Release Sentiment Distribution",
       fill = "Sentiments:") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "bottom"  # you can set to "right" to hide or "none" if you prefer
  )
# Thematic Tone for March Realses

March_tone <- get_nrc_sentiment(text_data)
tone_summary <- colSums(March_tone)
tone_df <- data.frame(Emotion = names(tone_summary), Count = tone_summary)

march_tone_df <- tone_df %>% 
  arrange(desc(Count)) %>% 
  mutate(
    percent = round(Count / sum(Count) * 100, 1),
    label = paste0(Emotion, ": ", percent, "%")
  )

# Emotion Bar Char

ggplot(march_tone_df, aes(x = reorder(Emotion, -percent), y = percent)) +  # Remove `fill = Emotion` from aes()
  geom_col(fill = "#AB892C") +  # Set static color for all bars
  theme_bw() +
  geom_text(aes(label = paste0(percent, "%")), vjust = -0.5) +
  labs(
    title = "MPFSS March Realease Tone Distribution", 
    x = "Tone Categories", 
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






emotions <- get_nrc_sentiment(text_data)
emotion_summary <- colSums(emotions)
emotion_df <- data.frame(Emotion = names(emotion_summary), Count = emotion_summary)

emotion_df <- emotion_df %>% 
  arrange(desc(Count)) %>% 
  mutate(
    percent = round(Count / sum(Count) * 100, 1),
    label = paste0(Emotion, ": ", percent, "%")
  )


# Sentiment Pie Chart
sentiment_summary <- sentiment_scores %>%
  mutate(sentiment_category = case_when(
    sentiment > 0 ~ "Positive",
    sentiment < 0 ~ "Negative",
    TRUE ~ "Neutral"
  )) %>%
  group_by(sentiment_category) %>%  # Group FIRST before counting
  summarise(
    count = n(),
    percent = round(n() / nrow(sentiment_scores) * 100, 1)
  ) %>%
  mutate(
    label = paste0(sentiment_category, ": ", percent, "%")
  )


# Create pie chart

ggplot(sentiment_summary, aes(x = "", y = percent, fill = sentiment_category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "#231F20", size = 4) +
  scale_fill_manual(values = c("Positive" = "#AB892C", "Negative" = "#753918", "Neutral" = "#FCF7EA")) +
  labs(title = "MPFSS2024 September Sentiment Distribution") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none"
  )

# Bar Chart

ggplot(sentiment_summary, aes(x = reorder(sentiment_category,-percent), y = percent, fill = sentiment_category)) +
  geom_col() +
  geom_text(aes(label = label), vjust = -0.5) +
  scale_fill_manual(values = c("Positive" = "#AB892C", "Negative" = "#753918", "Neutral" = "#FCF7EA"))+
  labs(title = "MPFSS2024 September Sentiment Distribution", x = "", y = "Percentage") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none"
  )

# Emotion Bar Chart
ggplot(emotion_df, aes(x = reorder(Emotion, -percent), y = percent, fill = Emotion)) +
  geom_col() +
  theme_bw() +
  geom_text(aes(label = paste0(percent,"%")), vjust = -0.5) +
  #scale_fill_manual(values = c("Positive" = "#AB892C", "Negative" = "#753918", "Neutral" = "#FCF7EA"))+
  labs(title = "MPFSS2024 September Sentiment Distribution", x = "", y = "Percentage") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none"
  )
#-----------------------------------------------------------------------------------------
#Complexity Level Index Analysis

import fitz  # PyMuPDF
import re

# Function to extract text from PDF
def extract_text_from_pdf(pdf_path):
  text = ""
with fitz.open(pdf_path) as doc:
  for page in doc:
  text += page.get_text()
return text

# Function to calculate Flesch Reading Ease score
def flesch_reading_ease(text):
  total_words = len(re.findall(r'\w+', text))
total_sentences = len(re.findall(r'[.!?]', text))
total_syllables = sum([len(re.findall(r'[aeiouy]+', word.lower())) for word in re.findall(r'\w+', text)])
    return 206.835 - 1.015 * (total_words / total_sentences) - 84.6 * (total_syllables / total_words)

# Function to calculate Flesch-Kincaid Grade Level
def flesch_kincaid_grade(text):
    total_words = len(re.findall(r'\w+', text))
    total_sentences = len(re.findall(r'[.!?]', text))
total_syllables = sum([len(re.findall(r'[aeiouy]+', word.lower())) for word in re.findall(r'\w+', text)])
    return 0.39 * (total_words / total_sentences) + 11.8 * (total_syllables / total_words) - 15.59

# Function to calculate Gunning Fog Index
def gunning_fog_index(text):
    total_words = len(re.findall(r'\w+', text))
    total_sentences = len(re.findall(r'[.!?]', text))
complex_words = len([word for word in re.findall(r'\w+', text) if len(re.findall(r'[aeiouy]+', word.lower())) > 2])
    return 0.4 * ((total_words / total_sentences) + 100 * (complex_words / total_words))

# Extract text from the provided PDF file
pdf_path = "MPFSS_March 2024_Final_Booklet 2.pdf"
pdf_text = extract_text_from_pdf(pdf_path)

# Calculate readability indices
flesch_reading_ease_score = flesch_reading_ease(pdf_text)
flesch_kincaid_grade_score = flesch_kincaid_grade(pdf_text)
gunning_fog_score = gunning_fog_index(pdf_text)

# Print the readability indices
print(f"Flesch Reading Ease: {flesch_reading_ease_score}")
print(f"Flesch-Kincaid Grade Level: {flesch_kincaid_grade_score}")
print(f"Gunning Fog Index: {gunning_fog_score}")
