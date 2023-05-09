# Script information ------------
#' Aim: Descriptive analysis of annotated English tweets on vaccine public perception 
#' Author: Laura Espinosa
#' Date created: 18 March 2023
#' Date updated: 18 March 2023

# Clean data -----------------------
message("Cleaning labelled tweets")
tweets <- read_csv("data/local/cleaned_labels_min-labels-cutoff-3_majority.csv")
tweets_clean <- tweets %>% 
  filter(question_tag == "vax_sentiment") %>% 
  select(-question_tag, -question_id, -label_id) %>% 
  mutate(label_tag = case_when(label_tag == "very_negative" | label_tag == "rather_negative" ~ "negative",
                               label_tag == "very_positive" | label_tag == "rather_positive" ~ "positive",
                               label_tag == "neutral" ~ "neutral",
                               TRUE ~ NA_character_),
         id = format(id, scientific = FALSE))

write_csv(tweets_clean, "data/local/labelled_tweets_clean.csv")

## Newest data ---------
tweets_new <- read_csv("data/local/new_tags_160221_cleaned_labels_min-labels-cutoff-3_majority.csv")
tweets_new_clean <- tweets_new %>% 
  filter(question_tag == "sentiment") %>% 
  select(-question_tag, -question_id, -label_id) %>% 
  mutate(label_tag = case_when(label_tag == "very_negative" | label_tag == "rather_negative" ~ "negative",
                               label_tag == "very_positive" | label_tag == "rather_positive" ~ "positive",
                               label_tag == "neutral" ~ "neutral",
                               TRUE ~ NA_character_),
         id = format(id, scientific = FALSE))

write_csv(tweets_new_clean, "data/local/labelled_tweets_new_clean.csv")

# Give me one word to select between neutral, positive and negative for the sentiment of this tweet:

## Including dates ------------
df <- read_csv("data/local/annotated_tweets_2.csv")

df_annotated <- df %>% 
  filter(id %in% tweets_clean$id)
