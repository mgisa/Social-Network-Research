# Source: https://www.rpubs.com/TeraPutera/social_network_analysis
#Create data frame from the networks
#-------------------------------------
# Data Wrangling
library(tidyverse) 
# For graph and visualization
library(tidygraph)
library(ggraph)
library(igraph)

# Read the data and Ensure created_at is in datetime format
mpfss_tweets <- read.csv("data/updated_usethis-MPFSS2024.csv") %>% 
  mutate(created_at = format(mdy_hm(created_at), "%Y-%m-%d %H:%M")) %>% 
  mutate(value_generated = 2 * retweet_count + #assigning weight on patterns
           1 * favorite_count +
           1.5 * retweet_retweet_count +
           1 * retweet_favorite_count)

#Creating retweets networks

rt_df<- mpfss_tweets[,c("screen_name","retweet_screen_name")]
#Source Vertex is the screen name and the target vertex is the retweet screen name
head(rt_df, n=3)
#Removing rows with missing values
rt_df_new<-rt_df[complete.cases(rt_df),]

# Replace empty strings or NA in screen_name with random values from retweet_screen_name
rt_df_new <- rt_df_new %>%
  mutate(screen_name = ifelse(
    screen_name == "" | is.na(screen_name),
    sample(retweet_screen_name[retweet_screen_name != "" & !is.na(retweet_screen_name)], 
           sum(screen_name == "" | is.na(screen_name)), 
           replace = TRUE),
    screen_name
  ))

#Convert to a matrix in order to construct network
matrx<-as.matrix(rt_df_new)
# Ensure the matrix is character type (required by graph_from_edgelist)
matrx <- as.matrix(rt_df_new[, c("screen_name", "retweet_screen_name")])
matrx <- matrx[complete.cases(matrx), ]  # Remove rows with NA
matrx <- matrx[matrx[,1] != "" & matrx[,2] != "", ]  # Remove rows with empty strings

#Select the distinct users

edge_df <- rt_df_new %>% 
  select(screen_name, retweet_screen_name) %>%  #Step 1
  separate_rows(retweet_screen_name, sep = " ") %>% #Step 2
  filter(retweet_screen_name != "") %>% #step 3
  rename(from = screen_name,
         to = retweet_screen_name) #Step 4

edge_df

nodes_df <- data.frame(name = unique(c(edge_df$from,edge_df$to)),
                       stringsAsFactors = F)

#Create twitter graph

graph_tweets <- tbl_graph(nodes = nodes_df,
                          edges = edge_df,
                          directed = F)

#Calculate Networks metrics

graph_tweets <- graph_tweets %>% 
  activate(nodes) %>%
  mutate(degree = centrality_degree(), # Degree centrality
         between = centrality_betweenness(normalized = T), # Betweeness centrality
         closeness = centrality_closeness(), # Closeness centrality
         eigen = centrality_eigen() # Eigen centrality
  ) %>% arrange(desc(degree))

#Identification of user who retweeted the most

#by calculating the out-degree of network
out_degreeNTW<-degree(graph_tweets,
                      mode = c("out"))

#Find the top three retweeted #MPFSS2024 the most.

out_degreeNTW_sort <-out_degreeNTW %>%
  sort(decreasing = TRUE)

out_degreeNTW_sort[1:6] #These users are key players and they can 
#be used as a medium to retweets and deseminating the communicated policy
View(out_degreeNTW_sort)
#________________________

#Identification of user whose post were retweeted most

#Calculate in-degree scores to identify the user whose post were retweeted most.
in_degree<-degree(graph_tweets,
                  mode = c("in"))


in_degree_sort<- in_degree%>% sort(decreasing = TRUE)
#Top five users whose posts were retweeted most.
in_degree_sort[1:6] # @JeanClaudeGaga is the influencer, it can be used to initiate the branding message of a firm

#Identify the users with high betweenness centrality.

#calculate the betweenness scores of the networks
btwn_scores<-betweenness(graph_tweets,directed = FALSE)
#Sorting users in descending order of betweenness scores
btwn_scores_sort<-btwn_scores%>%
  sort(decreasing = TRUE)%>%
  round(digits = 3)
#View the top three users who are key bridges btn people who retweet frequently and
#users whose retweets are tweeted frequently.
btwn_scores_sort[1:5]   #No connection between people who are retweets frequently 
#and users whose retweets are tweeted frequently.

# Extract vertex info
vertex_info <- as_data_frame(graph_tweets, what = "vertices")

#Convert graph data into data frame
network_act_df <- graph_tweets %>% 
  activate(nodes) %>% 
  as.data.frame()

pop_username <- data.frame(
  network_act_df %>% arrange(-degree) %>% select(name) %>% head(),
  network_act_df %>% arrange(-between) %>% select(name) %>% head(),
  network_act_df %>% arrange(-closeness) %>% select(name) %>% head(),
  network_act_df %>% arrange(-eigen) %>% select(name) %>% head()
) %>% setNames(c("Degree","Betweenness","Closeness","Eigen"))

pop_username

mpfss_tweets %>% 
  filter(retweet_screen_name == "JeanClaudeGaga") %>%
  group_by(retweet_screen_name, text) %>% 
  tally() %>% 
  arrange(-n) %>%
  pull(text) %>% 
  head(3)

#Graph Visualization
set.seed(12345)
graph_tweets <- graph_tweets %>% 
  activate(nodes) %>% 
  mutate(community = group_louvain()) %>% # clustering users
  activate(edges) %>% 
  filter(!edge_is_loop())  # Remove loop edges

graph_tweets %>% 
  activate(nodes) %>% 
  as.data.frame() #%>% 
  count(community)
#We have 3 communities and together active user in each community

important_user <- function(data) {
  name_person <- data %>%
    as.data.frame() %>% 
    filter(community %in% 1:3) %>% 
    select(-community) %>% 
    pivot_longer(-name, names_to = "measures", values_to = "values") %>% 
    group_by(measures) %>% 
    arrange(desc(values)) %>% 
    slice(1:6) %>% 
    ungroup() %>% 
    distinct(name) %>% 
    pull(name)
  
  return(name_person)
}

#create object contain important person
important_person <- 
  graph_tweets %>% 
  activate(nodes) %>% 
  important_user()

# Visualization using ggraph.
set.seed(12345)
graph_tweets %>%
  activate(nodes) %>%
  mutate(ids = row_number(),
         community = as.character(community)) %>%
  filter(community %in% 1:3) %>% # number of community.
  arrange(community,ids) %>%
  mutate(node_label = ifelse(name %in% important_person, name,NA)) %>%
  ggraph(layout = "fr") +
  #geom_edge_link(alpha = 0.3 ) +
  geom_edge_link(edge_width = 0.25, arrow = arrow(30, unit(.15, "cm"))) +
  geom_node_point(aes(size = degree, fill = community), shape = 21, alpha = 0.7, color = "grey30") +
  geom_node_label(aes(label = node_label), repel = T, alpha = 0.8 ) +
  guides(size = "none") +
  labs(title = "Top  Community of #MPFSS2024",
       color = "Interaction",
       fill = "Community") +
  theme_void() +
  theme(legend.position = "top")
#_--------------------------------
set.seed(12345)
graph_tweets %>%
  activate(nodes) %>%
  mutate(ids = row_number(),
         community = as.character(community)) %>%
  filter(community %in% 1:3) %>% # number of community
  arrange(community, ids)%>%
  mutate(node_label = ifelse(name %in% important_person, name, name),
         # Apply custom colors based on community
         node_color = case_when(
           name == "CentralBankRw" ~ "red",  # Center node in red
           community == "1" ~ "#753918",  # Dark brown
           community == "2" ~ "black",  # Olive gold
           community == "3" ~ "#DBA628",  # Golden yellow
           #community == "4" ~ "black",    # Black
           TRUE ~ "gray"  # Fallback color
         ))%>%
  ggraph(layout = "fr") +
  #geom_edge_link(alpha = 0.3, color = "gray70") +
  geom_edge_link(edge_width = 0.25, arrow = arrow(30, unit(.15, "cm")))+ #adding interaction direction
  geom_node_point(aes(size = 10*degree, fill = node_color), 
                  shape = 21, alpha = 0.9, color = "grey30") +
  geom_node_label(aes(label = node_label), 
                  repel = TRUE, alpha = 0.8, size = 3) +
  scale_fill_identity(guide = "legend", 
                      labels = c("@CentralBankRw", "Cluster1", "Cluster2", "Cluster3"),
                      breaks = c("red", "#753918", "black", "#DBA628")) +
  guides(size = "none",
         fill = guide_legend(override.aes = list(size = 5))) +
  labs(title = " #MPFSS2024 Top Communities and Engagement Network",
       fill = "Community") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.background = element_rect(fill = "#FCF7EA"),  # Graph area background
        panel.background = element_rect(fill = "#FCF7EA"), # Panel background
        panel.grid.major = element_line(color = "white"),  # Optional: light grid lines
        panel.grid.minor = element_line(color = "white")   # Optional: light grid lines
  )

# Visualization for all users

set.seed(123)
graph_tweets %>%
  activate(nodes) %>%
  mutate(ids = row_number(),
         community = as.character(community)) %>%
  filter(community %in% 1:4) %>% # Keep only the top 4 communities
  arrange(community, ids) %>%
  mutate(node_label = name) %>% # Show ALL usernames
  ggraph(layout = "fr") +
  geom_edge_link(alpha = 0.3, color = "gray70") +
  geom_node_point(aes(size = degree, fill = community), 
                  shape = 21, alpha = 0.7, color = "grey30") +
  geom_node_label(aes(label = node_label), 
                  repel = TRUE, alpha = 0.8, size = 2, max.overlaps = 100) + # Adjust label size
  guides(size = "none") +
  labs(title = "Full Network of #MPFSS2024 (All Users)",
       fill = "Community") +
  theme_void() +
  theme(legend.position = "top")


#----------------------CREATE FINAL DF FOR MOORA---------------

# Combine tweet data with network metrics

user_data <- mpfss_tweets %>%
  group_by(retweet_screen_name) %>%
  summarise(
    tweet_count = n(),
    favorite_count = sum(favorite_count, na.rm = TRUE),
    retweet_count = round(sum(retweet_count, na.rm = TRUE), digits = 0),
    followers = first(followers_count),
    following = first(friends_count)
  ) %>%
  left_join(vertex_info, by = c("retweet_screen_name" = "name")) %>%
  mutate(
    influence_score = (degree / max(degree, na.rm = TRUE) * 0.4 + 
                         (between / max(between, na.rm = TRUE)) * 0.3 +
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
    favorite_count,
    retweet_count,
    followers,
    following,
    degree,
    betweenness = between,
    closeness,
    eigen_centrality = eigen,
    # community,
    avg_sentiment,
    pos_tweets,
    neg_tweets,
    influence_score,
    community
  ) %>%
  arrange(desc(influence_score))

write.csv(final_matrix, "data/MOORA_datamatrx.csv", row.names = F)
