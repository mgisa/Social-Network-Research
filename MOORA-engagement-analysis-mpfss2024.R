#MOORA based MPFSS2024 Engagement Emplifier
#___________________________________________

# Load required libraries
library(dplyr)
library(ggplot2)
library(scales)
library(xtable)

# Load the data
moora_df <- read.csv("data/MOORA_datamatrx.csv", stringsAsFactors = FALSE)

# Remove rows with missing user names
moora_df <- moora_df %>% filter(retweet_screen_name != "")

# Select relevant columns for MOORA
moora_criteria <- moora_df %>%
  select(retweet_screen_name,
         avg_sentiment, 
         retweet_count,
         favorite_count, 
         influence_score)

# Normalize the criteria (benefit criteria)
moora_normalized <- moora_criteria %>%
  mutate(across(-retweet_screen_name, ~ . / sqrt(sum(.^2, na.rm = TRUE))))

# Calculate MOORA score (equal weights)
moora_normalized <- moora_normalized %>%
  rowwise() %>%
  mutate(MOORA_score = sum(c_across(-retweet_screen_name))) %>%
  ungroup()

# Rank users
moora_ranked <- moora_normalized %>%
  arrange(desc(MOORA_score)) %>%
  mutate(Rank = row_number())


# Convert MOORA score to percentage
moora_ranked <- moora_ranked %>%
  mutate(MOORA_score_pct = round((MOORA_score/sum(MOORA_score)) * 100, digits = 2))



# View top 10 amplifiers
top_10 <- moora_ranked %>% slice(1:10)

# Plot top 10 amplifiers (Score Plot)
ggplot(top_10, aes(x = reorder(paste('@',retweet_screen_name,sep =''), MOORA_score), y = MOORA_score)) +
  geom_bar(stat = "identity", fill = "#753918") +
  coord_flip() +
  labs(title = "Top 10 Amplifiers of #MPFSS2024 Messages",
       x = "User", y = "MOORA Score") +
  theme_minimal()

#Percentage Plot

ggplot(top_10, aes(x = reorder(paste0("@", retweet_screen_name,sep =''), MOORA_score_pct), 
                   y = MOORA_score_pct)) +
  geom_bar(stat = "identity", fill = "#753918", width = 0.7) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+  # Remove padding on left
  geom_text(aes(label = paste0(round(MOORA_score_pct, 1), "%")), 
            hjust = 1.1, 
            color = "white", 
            size = 3.5,
            fontface = "bold") +
  coord_flip() +
  labs(title = "Top 10 Amplifiers of #MPFSS2024 Communique",
       x = "", y = "Engagement Score (%)") +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        plot.background = element_rect(fill = "#FCF7EA"),  # Graph area background
        panel.background = element_rect(fill = "#FCF7EA"), # Panel background
        panel.grid.major = element_line(color = "white"),  # Optional: light grid lines
        panel.grid.minor = element_line(color = "white")   # Optional: light grid lines
  )


# Export LaTeX table
latex_table <- xtable(top_10 %>% select(User = retweet_screen_name, `MOORA Score` = MOORA_score, Rank))
print(latex_table, include.rownames = FALSE, file = "top_10_amplifiers.tex")
